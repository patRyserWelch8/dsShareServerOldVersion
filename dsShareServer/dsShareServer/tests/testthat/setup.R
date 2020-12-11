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

assign("D", read.csv("data_files/DATASET1.csv", header = TRUE), pos = 1)
assign("E", read.csv("data_files/DATASET2.csv", header = TRUE), pos = 1)
assign("F", read.csv("data_files/DATASET3.csv", header = TRUE), pos = 1)





