



data1 <- read.csv("all_data.csv")
head(data1)
x_pi <- 3.14159265358979324 * 3000.0 / 180.0
pi <- 3.1415926535897932384626#π
a <- 6378245.0#长半轴
ee <- 0.00669342162296594323#扁率


#百度坐标系(BD-09)转火星坐标系(GCJ-02)
data2<- data1[complete.cases(data1$longitude),]
num <- length(data2$shop_name.x)

jwd_data <- data.table()
for(i in 1:num){
data3 <- data1[i,]
shop_adr_net <- data3$shop_adr_net
bd_lng <- data3$longitude
bd_lat <- data3$latitude

x <- bd_lng - 0.0065
y <- bd_lat - 0.006
z <- sqrt(x * x + y * y) - 0.00002 * sin(y * x_pi)
theta <- atan2(y, x) - 0.000003 * cos(x * x_pi)
lng <- z * cos(theta)
lat <- z * sin(theta)


#GCJ02(火星坐标系)转GPS84
#纬度转换函数
transformlat <- function(lng, lat) {
        ret <- -100.0 + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat +
                0.1 * lng * lat + 0.2 * sqrt(abs(lng))
        + (20.0 * sin(6.0 * lng * pi) + 20.0 *
                   sin(2.0 * lng * pi)) * 2.0 / 3.0
        + (20.0 * sin(lat * pi) + 40.0 *
                   sin(lat / 3.0 * pi)) * 2.0 / 3.0
        + (160.0 * sin(lat / 12.0 * pi) + 320 *
                   sin(lat * pi / 30.0)) * 2.0 / 3.0
        return(ret)
}
#纬经度转换函数
transformlng <- function(lng, lat) {
        ret <- 300.0 + lng + 2.0 * lat + 0.1 * lng * lng +
                0.1 * lng * lat + 0.1 * sqrt(abs(lng))
        + (20.0 * sin(6.0 * lng * pi) + 20.0 *
                   sin(2.0 * lng * pi)) * 2.0 / 3.0
        + (20.0 * sin(lng * pi) + 40.0 *
                   sin(lng / 3.0 * pi)) * 2.0 / 3.0
        + (150.0 * sin(lng / 12.0 * pi) + 300.0 *
                   sin(lng / 30.0 * pi)) * 2.0 / 3.0
        return(ret)
}

#火星坐标转换为WGS84
dlat <- transformlat(lng - 105.0, lat - 35.0)
dlng <- transformlng(lng - 105.0, lat - 35.0)
radlat <- lat / 180.0 * pi
magic <- sin(radlat)
magic <- 1 - ee * magic * magic
sqrtmagic <- sqrt(magic)
dlat <- (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
dlng <- (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
mglat <- lat + dlat
mglng <- lng + dlng
jwd_lng <- lng * 2 - mglng
jwd_lat <- lat * 2 - mglat

jwd_data1 <- data.table(shop_adr_net,jwd_lng,jwd_lat)
jwd_data <- rbind(jwd_data,jwd_data1)
}
write.csv(jwd_data, "jwd_data.csv")
