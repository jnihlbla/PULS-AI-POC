000100*** EDIT ALLOWED                                                          
000200 01  WDIRBBD0.                                                            
000300*                                 DIRECT BUSINESS,                        
000400*                                 BILLING RECORD (DEBIT).                 
000500*                                                                         
000600     03 B0-PROGRAM           PIC X(4).                                    
000700*                                                                         
000800     03 B0-FILEDEFNO         PIC X(2).                                    
000900*                                                                         
001000     03 B0-SENDLOC           PIC X(3).                                    
001100*                                                                         
001200     03 B0-RECLOC            PIC X(3).                                    
001300*                                                                         
001400     03 B0-SERIALNO          PIC X(7).                                    
001500*                                                                         
001600     03 B0-RECORDTYPE        PIC X(1).                                    
001700*                                                                         
001800*                                                                         
001900     03 B0-BILL-LOC          PIC X(3).                                    
002000*                                                                         
002100     03 B0-CUST-ACC-CODE     PIC X(7).                                    
002200*                                                                         
002300     03 FILLER               PIC X(1).                                    
002400*                                                                         
002500     03 B0-CUST-ORDERING     PIC X(7).                                    
002600*                                                                         
002700     03 B0-RETAIL-DEALER     PIC X(7).                                    
002800*                                                                         
002900     03 B0-DEL-NOTENO        PIC X(9).                                    
003000*                                                                         
003100     03 B0-LINETYPE          PIC 9(1).                                    
003200*                                                                         
003300     03 B0-LINEDATA          PIC X(105).                                  
003400*                                                                         
003500     03 B0-ORDERTEXT REDEFINES B0-LINEDATA.                               
003600*                                                                         
003700        05 B0-ORDERTEXT-1 PIC X(50).                                      
003800*                                                                         
003900        05 B0-ORDERTEXT-2 PIC X(50).                                      
004000*                                                                         
004100        05 FILLER            PIC X(4).                                    
004101*                                                                         
004110        05 B0-TEXTCOUNT-1    PIC 9(1).                                    
004200*                                                                         
004300     03 B0-DETAIL REDEFINES B0-LINEDATA.                                  
004400*                                                                         
004500        05 B0-PARTNO         PIC X(12).                                   
004600*                                                                         
004700        05 B0-UNIT-PRICE     PIC 9(9).                                    
004800*                                                                         
004900        05 B0-QUANTITY       PIC 9(5).                                    
005000*                                                                         
005100        05 B0-LINE-VALUE     PIC 9(11).                                   
005200*                                                                         
005300        05 FILLER            PIC X(1).                                    
005400*                                                                         
005500        05 B0-NOCHARGE       PIC X(1).                                    
005600*                                                                         
005700        05 B0-FILE-CONTR     PIC 9(6).                                    
005800*                                                                         
006100        05 B0-DELNOTEDATE PIC 9(8).                                       
006101*                                                                         
006110        05 B0-LINE-CONTR     PIC 9(4).                                    
006200*                                                                         
006300        05 FILLER            PIC X(11).                                   
006400*                                                                         
006500        05 B0-INVDUEDATE     PIC 9(8).                                    
006600*                                                                         
006610        05 B0-ORDER-DATE     PIC 9(8).                                    
006620*                                                                         
006630        05 B0-PRODQ-1        PIC X(10).                                   
006640*                                                                         
006650        05 B0-PRODQ-2        PIC X(10).                                   
006660*                                                                         
006700        05 FILLER            PIC X(1).                                    
006800*                                                                         
006900     03 B0-LINETEXT REDEFINES B0-LINEDATA.                                
007000*                                                                         
007100        05 B0-LINETEXT-1.                                                 
007200*                                                                         
007300          07 B0-SUPPLIER     PIC X(3).                                    
007400*                                                                         
007500          07 FILLER          PIC X(1).                                    
007600*                                                                         
007700          07 B0-BRAND        PIC X(3).                                    
007800*                                                                         
007900          07 FILLER          PIC X(1).                                    
008000*                                                                         
008100          07 B0-ITEMDESC     PIC X(40).                                   
008200*                                                                         
008210          07 FILLER          PIC X(2).                                    
008220*                                                                         
008300        05 B0-LINETEXT-2.                                                 
008310*                                                                         
008320          07 B0-DISC1        PIC X(5).                                    
008330*                                                                         
008340          07 FILLER          PIC X(2).                                    
008400*                                                                         
008410          07 B0-DISC2        PIC X(5).                                    
008420*                                                                         
008430          07 FILLER          PIC X(2).                                    
008440*                                                                         
008450          07 B0-DISC3        PIC X(5).                                    
008460*                                                                         
008470          07 FILLER          PIC X(2).                                    
008480*                                                                         
008490          07 B0-DISC4        PIC X(5).                                    
008491*                                                                         
008492          07 FILLER          PIC X(4).                                    
008493*                                                                         
008494          07 B0-LISTPRICE PIC X(10).                                      
008495*                                                                         
008496          07 FILLER          PIC X(10).                                   
008497*                                                                         
008498        05 B0-PRODTYPE       PIC X(1).                                    
008499*                                                                         
008500        05 FILLER            PIC X(3).                                    
008501*                                                                         
008510        05 B0-TEXTCOUNT-2    PIC 9(1).                                    
008600*                                                                         
008700*** END OF VILMAII-COPY LENGTH=160                                        
