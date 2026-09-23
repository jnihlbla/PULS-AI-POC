000100*** EDIT ALLOWED                                                          
000200 01  WINVBCS0.                                                            
000300*                                 DIRECT BUSINESS,                        
000400*                                 CUSTOMER BASIC DATA.                    
000500*                                                                         
000600     03 S0-PROGRAM           PIC X(4).                                    
000700*                                                                         
000800     03 S0-FILEDEFNO         PIC X(2).                                    
000900*                                                                         
001000     03 S0-SENDLOC           PIC X(3).                                    
001100*                                                                         
001200     03 S0-RECLOC            PIC X(3).                                    
001300*                                                                         
001400     03 S0-SERIALNO          PIC X(7).                                    
001500*                                                                         
001600     03 S0-RECORDTYPE        PIC X(1).                                    
001700*                                                                         
001800*                                                                         
001900     03 S0-BILL-LOC          PIC X(3).                                    
002000*                                                                         
002010     03 S0-CUST-ORDERING     PIC X(7).                                    
002110*                                                                         
002120     03 S0-SUBRECORD-TYPE    PIC X(1).                                    
002121*                                                                         
002130     03 S0-SUBRECORD-DATA    PIC X(193).                                  
002200*                                                                         
002300     03 S0-BILL-TO-ADDRESS REDEFINES S0-SUBRECORD-DATA.                   
002400*                                                                         
002500       05 S0-BILL-ADDR-1     PIC X(30).                                   
002600*                                                                         
002610       05 S0-BILL-ADDR-2     PIC X(30).                                   
002620*                                                                         
002630       05 S0-BILL-ADDR-3     PIC X(30).                                   
002640*                                                                         
002650       05 S0-BILL-ADDR-4     PIC X(30).                                   
002660*                                                                         
002670       05 S0-BILL-ADDR-5     PIC X(30).                                   
002680*                                                                         
002690       05 S0-BILL-ADDR-6     PIC X(30).                                   
002691*                                                                         
002692       05 FILLER             PIC X(13).                                   
002693*                                                                         
002694     03 S0-SHIP-TO-ADDRESS REDEFINES S0-SUBRECORD-DATA.                   
002695*                                                                         
002696       05 S0-SHIP-ADDR-1     PIC X(30).                                   
002697*                                                                         
002698       05 S0-SHIP-ADDR-2     PIC X(30).                                   
002699*                                                                         
002700       05 S0-SHIP-ADDR-3     PIC X(30).                                   
002701*                                                                         
002702       05 S0-SHIP-ADDR-4     PIC X(30).                                   
002703*                                                                         
002704       05 S0-SHIP-ADDR-5     PIC X(30).                                   
002705*                                                                         
002706       05 S0-SHIP-ADDR-6     PIC X(30).                                   
002707*                                                                         
002708       05 FILLER             PIC X(13).                                   
002709*                                                                         
002710     03 S0-DEALER-BASIC REDEFINES S0-SUBRECORD-DATA.                      
002711*                                                                         
002712       05 S0-CUST-ACC-CODE   PIC X(7).                                    
002800*                                                                         
002810       05 S0-CREDIT-RATE     PIC X(1).                                    
002820*                                                                         
002830       05 FILLER             PIC X(1).                                    
002840*                                                                         
002850       05 S0-SALES-REG       PIC 9(1).                                    
002860*                                                                         
002870       05 S0-SALES-MARK-A    PIC X(2).                                    
002880*                                                                         
002890       05 S0-SALES-DISTR     PIC X(2).                                    
002891*                                                                         
002892       05 S0-SALES-ZONE      PIC X(2).                                    
002893*                                                                         
002894       05 S0-CUST-TYPE       PIC X(1).                                    
002895*                                                                         
002896       05 FILLER             PIC X(6).                                    
002897*                                                                         
002898       05 S0-EXPIRE-DATE     PIC 9(8).                                    
002899*                                                                         
002900       05 FILLER             PIC X(162).                                  
002901*                                                                         
008700*** END OF VILMAII-COPY LENGTH=224                                        
