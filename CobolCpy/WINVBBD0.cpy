000100*** EDIT ALLOWED                                                          
000200 01  WDIRBBA0.                                                            
000300*                                 DIRECT BUSINESS,                        
000400*                                 BILLING RECORD (ADJUSTMENT).            
000500*                                                                         
000600     03 D0-PROGRAM           PIC X(4).                                    
000700*                                                                         
000800     03 D0-FILEDEFNO         PIC X(2).                                    
000900*                                                                         
001000     03 D0-SENDLOC           PIC X(3).                                    
001100*                                                                         
001200     03 D0-RECLOC            PIC X(3).                                    
001300*                                                                         
001400     03 D0-SERIALNO          PIC X(7).                                    
001500*                                                                         
001600     03 D0-RECORDTYPE        PIC X(1).                                    
001700*                                                                         
001800*                                                                         
001900     03 D0-BILL-LOC          PIC X(3).                                    
002000*                                                                         
002100     03 D0-CUST-ACC-CODE     PIC X(7).                                    
002200*                                                                         
002300     03 FILLER               PIC X(1).                                    
002400*                                                                         
002500     03 D0-CUST-ORDERING     PIC X(7).                                    
002600*                                                                         
002700     03 D0-RETAIL-DEALER     PIC X(7).                                    
002800*                                                                         
002900     03 D0-DEL-NOTENO        PIC X(9).                                    
003000*                                                                         
003100     03 D0-LINETYPE          PIC 9(1).                                    
003200*                                                                         
003300     03 D0-LINEDATA          PIC X(105).                                  
003400*                                                                         
003500     03 D0-ORDERTEXT REDEFINES D0-LINEDATA.                               
003600*                                                                         
003700        05 D0-ORDERTEXT-1 PIC X(50).                                      
003800*                                                                         
003900        05 D0-ORDERTEXT-2 PIC X(50).                                      
004000*                                                                         
004100        05 FILLER            PIC X(4).                                    
004101*                                                                         
004110        05 D0-TEXTCOUNT-1    PIC 9(1).                                    
004200*                                                                         
004300     03 D0-DETAIL REDEFINES D0-LINEDATA.                                  
004400*                                                                         
004500        05 D0-PARTNO         PIC X(12).                                   
004600*                                                                         
004700        05 D0-UNIT-PRICE     PIC 9(9).                                    
004800*                                                                         
004900        05 D0-QUANTITY       PIC 9(5).                                    
005000*                                                                         
005100        05 D0-LINE-VALUE     PIC 9(11).                                   
005200*                                                                         
005300        05 FILLER            PIC X(1).                                    
005400*                                                                         
005500        05 D0-DEB-CRED       PIC X(1).                                    
005600*                                                                         
005700        05 D0-FILE-CONTR     PIC 9(6).                                    
006000*                                                                         
006100        05 D0-CREATEDATE     PIC 9(8).                                    
006200*                                                                         
006210        05 D0-LINE-CONTR     PIC 9(4).                                    
006220*                                                                         
006300        05 D0-ORIG-INVNO     PIC X(10).                                   
006400*                                                                         
006500        05 FILLER            PIC X(17).                                   
006600*                                                                         
006700        05 D0-PRODQ-1        PIC X(10).                                   
006800*                                                                         
006810        05 D0-PRODQ-2        PIC X(10).                                   
006820*                                                                         
006900        05 FILLER            PIC X(1).                                    
007000*                                                                         
007100     03 D0-LINETEXT REDEFINES D0-LINEDATA.                                
007200*                                                                         
007300        05 D0-LINETEXT-1.                                                 
007400*                                                                         
007500          07 D0-SUPPLIER     PIC X(3).                                    
007600*                                                                         
007700          07 FILLER          PIC X(1).                                    
007800*                                                                         
007900          07 D0-BRAND        PIC X(3).                                    
008000*                                                                         
008100          07 FILLER          PIC X(1).                                    
008200*                                                                         
008300          07 D0-ITEMDESC     PIC X(40).                                   
008400*                                                                         
008410          07 FILLER          PIC X(2).                                    
008420*                                                                         
008500        05 D0-LINETEXT-2.                                                 
008600*                                                                         
008610          07 D0-DISC1        PIC X(5).                                    
008620*                                                                         
008630          07 FILLER          PIC X(2).                                    
008640*                                                                         
008650          07 D0-DISC2        PIC X(5).                                    
008660*                                                                         
008670          07 FILLER          PIC X(2).                                    
008680*                                                                         
008690          07 D0-DISC3        PIC X(5).                                    
008691*                                                                         
008692          07 FILLER          PIC X(2).                                    
008693*                                                                         
008694          07 D0-DISC4        PIC X(5).                                    
008695*                                                                         
008696          07 FILLER          PIC X(4).                                    
008697*                                                                         
008698          07 D0-LISTPRICE    PIC X(10).                                   
008699*                                                                         
008700          07 FILLER          PIC X(10).                                   
008701*                                                                         
008702        05 D0-PRODTYPE       PIC X(1).                                    
008703*                                                                         
008710        05 FILLER            PIC X(3).                                    
008711*                                                                         
008720        05 D0-TEXTCOUNT-2    PIC 9(1).                                    
008800*                                                                         
008900*** END OF VILMAII-COPY LENGTH=160                                        
