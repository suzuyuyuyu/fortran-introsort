#define CHECK_SORTED(x) x(1:size(x)-1) <= x(2:size(x))
#define CHECK_STABLE(x, y) x(1:size(x)-1) <= x(2:size(x)) .and. (x(1:size(x)-1) < x(2:size(x)) .or. (x(1:size(x)-1) == x(2:size(x)) .and. y(1:size(y)-1) < y(2:size(y))))
#define IS_SORTED(x) all(x(1:size(x)-1) <= x(2:size(x)))
#define IS_STABLE(x, y) all(x(1:size(x)-1) <= x(2:size(x)) .and. (x(1:size(x)-1) < x(2:size(x)) .or. (x(1:size(x)-1) == x(2:size(x)) .and. y(1:size(y)-1) < y(2:size(y)))))

#define IS_STRICTLY_INCREASING(x) all(x(1:size(x)-1) < x(2:size(x)))
