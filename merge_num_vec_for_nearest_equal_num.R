# Merge two numeric vectors and display closest number
a = data.table::data.table((Value = as.numeric(colnames(select(FloX, !c("doy.dayfract", "datetime", "Plot"))))))
a[,merge:=Value]
b = data.table::data.table(Value = as.numeric(band_760_FloX))
b[,merge:=Value]
data.table::setkeyv(a, c("merge"))
data.table::setkeyv(b, c("merge"))
Merge_a_b = a[b, roll = "nearest"]
