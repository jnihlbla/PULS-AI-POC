000100 01  POST.                                                                
000200*                                 SORTAREA FR≈N PROGRAM W3719000          
000300*                                           PLUS                          
000400*                                 WDA801-COPYTEXTEN                       
000500*                                                                         
000600     03 IDDISTR-SORT         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR-SORT        PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDARTNR-SORT         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 TIAAMMDD-REG-SORT    PIC S9(7)           COMP-3.                  
001300*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001400     03 KDOBJEKT-SORT        PIC S9              COMP-3.                  
001500*                                 OBJEKTSKOD                              
001600     03 IDORDNR-SORT         PIC S9(5)           COMP-3.                  
001700*                                 ORDERNUMMER                             
001800     03 IDPTYP-SORT          PIC X(3).                                    
001900*                                 POSTTYP                                 
002000     03 WDA801-KEY.                                                       
002100        05 IDDISTR           PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
002400*                                 KUNDNUMMER                              
002500        05 IDARTNR           PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700     03 FLRESK               PIC X.                                       
002800*                                 RESKONTRAAVBOKARE ?                     
002900*                                 (ANNARS ORDERAVBOKARE)                  
003000     03 KVRETUR              PIC S9(7)           COMP-3.                  
003100*                                 ANTAL I RETUR                           
003200*** END COPY W3719501    LENGTH=40                                        
