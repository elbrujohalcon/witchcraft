-module(invoices).

-record(invoice,
    { id       :: binary()
    , date     :: calendar:datetime()
    , customer :: binary()
    , address  :: undefined|binary()
    , amount   :: number()
    , archived :: undefined|calendar:datetime()
    }).

-opaque invoice() :: #invoice{}.
-export_type([invoice/0]).

-export(
  [new/2, amount/1, address/1, archived/2]).

-spec new(binary(), number()) -> invoice().
new(Customer, Amount) when Amount > 0 ->
  #invoice{ id       = uuid:new()
          , date     = calendar:universal_time()
          , customer = Customer
          , amount   = Amount
          }.

-spec amount(invoice()) -> number().
amount(#invoice{amount = Amount}) -> Amount.

-spec address(invoice()) -> undefined|binary().
address(#invoice{address = Address}) -> Address.

-spec archived(invoice(), calendar:datetime())
      -> invoice().
archived(Invoice, Date) ->
  Invoice#invoice{archived = Date}.
