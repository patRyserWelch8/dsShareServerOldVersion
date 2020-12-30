#-------------------------------------------------------------------------------
# Copyright (c) 2019 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------
#
# Datashield test suite set up
#
#library(R6)
#library(ds.server.wrapping.R.functions)


print("setup.r")

vector_A <- c(1:100)
vector_B <- vector_A * 100000
vector_C <- vector_A + 1

matrix_A <- matrix(vector_A, 10, 10)
matrix_B <- matrix(vector_B, 5, 20)
matrix_C <- matrix(vector_C, 10, 10)

list_A   <- list(vector_A)
list_B   <- list(vector_B)
list_C   <- list(vector_C)

df_A     <- data.frame(vector_A,vector_B, list_C)
df_B    <- data.frame(vector_A * -1000 , vector_B * -1000, vector_C * -1000)
df_C     <- data.frame(vector_A + 1, vector_B + 1, vector_C + 1)

assign("vector_A", vector_A, pos = 1)
assign("vector_B", vector_B, pos = 1)
assign("vector_C", vector_C, pos = 1)

assign("matrix_A", matrix_A, pos = 1)
assign("matrix_B", matrix_B, pos = 1)
assign("matrix_C", matrix_C, pos = 1)

assign("list_A", list_A, pos = 1)
assign("list_B", list_B, pos = 1)
assign("list_C", list_C, pos = 1)

assign("df_A", df_A, pos = 1)
assign("df_B", df_B, pos = 1)
assign("df_C", df_C, pos = 1)

assign("D", read.csv("data_files/DATASET1.csv", header = TRUE), pos = 1)
assign("E", read.csv("data_files/DATASET2.csv", header = TRUE), pos = 1)
assign("F", read.csv("data_files/DATASET3.csv", header = TRUE), pos = 1)






