apply = \f -> \x -> f x;

const : forall a b. a -> b -> a;
const = \a -> \b -> a
