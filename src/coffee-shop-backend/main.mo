import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Text "mo:base/Text";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Hash "mo:base/Hash";
import Nat32 "mo:base/Nat32";
import Principal "mo:base/Principal";
import Error "mo:base/Error";

actor CoffeeShop {
  type Coffee = {
    id : Nat;
    name : Text;
    price : Nat;
    stock : Nat;
  };

  type Order = {
    id : Nat;
    items : [Coffee];
    totalPrice : Nat;
    customer : Principal;
  };

  let owner : Principal = Principal.fromActor(CoffeeShop);
  var menu = HashMap.HashMap<Nat, Coffee>(10, Nat.equal, func(n : Nat) : Hash.Hash { Nat32.fromNat(n) });
  var orders = HashMap.HashMap<Nat, Order>(10, Nat.equal, func(n : Nat) : Hash.Hash { Nat32.fromNat(n) });
  var nextCoffeeId = 0;
  var nextOrderId = 0;

  public shared (message) func onlyOwner() : async () {
    if (Principal.equal(message.caller, owner)) {
      return ();
    } else {
      throw Error.reject("Only the owner can perform this action.");
    };
  };

  public func addCoffee(name : Text, price : Nat, stock : Nat) : async Coffee {
    await onlyOwner();

    let newCoffee = {
      id = nextCoffeeId;
      name = name;
      price = price;
      stock = stock;
    };

    menu.put(nextCoffeeId, newCoffee);
    nextCoffeeId += 1;

    return newCoffee;
  };

  public query func getMenu() : async [Coffee] {
    return Iter.toArray(menu.vals());
  };

  public shared (message) func placeOrder(coffeeIds : [Nat]) : async ?Order {
    var orderItems : [Coffee] = [];
    var totalPrice : Nat = 0;

    for (coffeeId in coffeeIds.vals()) {
      let coffeeOpt = menu.get(coffeeId);

      switch (coffeeOpt) {
        case (?coffee) {
          if (coffee.stock <= 0) {
            return null;
          };

          orderItems := Array.append(orderItems, [coffee]);
          totalPrice += coffee.price;

          // Update stock in the menu
          menu.put(coffee.id, { coffee with stock : Nat = coffee.stock - 1 });
        };
        case (_) { return null };
      };
    };

    let newOrder : Order = {
      id = nextOrderId;
      items = orderItems;
      totalPrice = totalPrice;
      customer = message.caller;
    };

    orders.put(nextOrderId, newOrder);
    nextOrderId += 1;

    return ?newOrder;
  };

  public func getOrders() : async [Order] {
    await onlyOwner();
    return Iter.toArray(orders.vals());
  };
};
