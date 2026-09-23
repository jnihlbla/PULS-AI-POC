000100 01  4002-WDGX4002.                                                       
000200*                                 PLOCKSATSER SOM VALTS                   
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400     03 4002-KDSEGKEY        PIC X.                                       
000500*                                 TEKNISK SEGMENT-NYCKEL                  
000600*                                 TECHNICAL SEGMENT KEY                   
000700     03 4002-FLORDKNY        PIC X.                                       
000800*                                 KNYTER ORDER ELLER ORDERDEL             
000900*                                 TILL EN PLOCKARE                        
001000*                                 ANVÄNDS NÄR INTE HELA ORDERN            
001100*                                 PLOCKAS SAMTIDIGT                       
001200*                                 J=HELA ORDERDELEN TILL                  
001300*                                 SAMMA PACKARE                           
001400*                                 N=VEM SOM HELST FÅR TA UT               
001500*                                 RESTERANDE RADER                        
001600*                                 TAKE ORDER OR PART OF ORDER             
001700*                                 TO A PICKER. YOU USE IT WHEN            
001800*                                 THE WHOLE ORDER IS NOT PICKING          
001900*                                 AT THE SAME TIME                        
002000*                                 J=THE WHOLE PART OF ORDER TO            
002100*                                 THE SAME PICKER                         
002200*                                 N=ANY PICKER CAN TAKE THE REST          
002300*                                 OF THE LINES                            
002400     03 4002-FLORDSPL        PIC X.                                       
002500*                                 SPLIT FLAGGA                            
002600*                                 SPLIT FLAG                              
002700     03 4002-IDBORD          PIC X(3).                                    
002800*                                 PACK-BORD                               
002900*                                 PACKING TABLE                           
003000     03 4002-IDLOPNR-PL      PIC S9(3)           COMP-3.                  
003100*                                 PLOCKSATSENS LÖPNUMMER INOM             
003200*                                 PRC-GRUPP                               
003300*                                 SEQUENCE-NUMBER FOR THE                 
003400*                                 PICKING UNIT WITHIN PRC-GROUP           
003500     03 4002-IDUSER          PIC X(8).                                    
003600*                                 ANVÄNDARENS SÄKERHETS ID                
003700*                                 USER SECURITY-IDENTITY                  
003800     03 4002-IXHEL           PIC S9(9)           COMP.                    
003900*                                 INDEX HELORD                            
004000*                                 INDEX FULLWORD                          
004100     03 4002-KDPRT-PLE       PIC X(3).                                    
004200*                                 PRINTERKOD PLOCKETIKETTER               
004300*                                 PRINTERCODE PICKING LABLES              
004400     03 4002-KDPRT-PU        PIC X(3).                                    
004500*                                 PRINTERKOD PACKUNDERLAG                 
004600*                                 PRINTERCODE PACKING UNIT                
004700     03 4002-KDSORT          PIC X(2).                                    
004800*                                 SORT-KOD                                
004900*                                 UNIT OF MEASURE                         
005000     03 4002-KVORDSPL        PIC S9(7)V9(1).                              
005100*                                 SPLITGRÄNS                              
005200*                                 SPLIT LIMIT                             
005300     03 4002-KVRADER         PIC S9(5)           COMP-3.                  
005400*                                 ANTAL RADER                             
005500*                                 NUMBER OF LINES                         
005600     03 4002-ORDDEL          OCCURS 99 TIMES.                             
005700        05 4002-IDORDER      PIC S9(7)           COMP-3.                  
005800*                                 VOLVO PARTS ORDERNUMMER                 
005900*                                 VOLVO PARTS ORDER NUMBER                
006000        05 4002-IDDC         PIC X(2).                                    
006100*                                 IDENTIFIERARE LAGER                     
006200*                                 WAREHOUSE IDENTIFIER                    
006300        05 4002-IDPRODNR     PIC S9(7)           COMP-3.                  
006400*                                 PRODUKTIONSNUMMER                       
006500*                                 PRODUCTION NUMBER                       
006600        05 4002-IDPLKLST     PIC S9(3)           COMP-3.                  
006700*                                 PLOCKLISTNUMMER                         
006800*                                 PICKING LIST NUMBER                     
006900     03 4002-SUORDV          PIC S9(9)V9(2)      COMP-3.                  
007000*                                 SUMMA ORDERVÄRDE                        
007100*                                 TOTAL ORDER VALUE                       
007200     03 4002-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
007300*                                 ORDERVIKT NETTO (KG)                    
007400*                                 WEIGHT PER ORDER NETTO (KG)             
007500     03 4002-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
007600*                                 ORDERVOLYM NETTO (M3)                   
007700*                                 NET VOLUME PER ORDER (M3)               
007800     03 4002-DEAL-PR-SUM.                                                 
007900*                                 DEALERPRIS (HUVUD)                      
008000        05 4002-SUORDV-LOC   PIC S9(9)V9(2)      COMP-3.                  
008100*                                 ORDERVÄRDE SLUTKUNDPRIS                 
008200*                                 I LOKAL VALUTA                          
008300*                                 ORDER VALUE, CUSTOMER PRICE             
008400*                                 IN LOCAL CURRENCY                       
008500        05 4002-SUORDV-LOCPREL                                            
008600                             PIC S9(9)V9(2)      COMP-3.                  
008700*                                 ORDERVÄRDE PREL SLUT-                   
008800*                                 KUNDPRIS, LOKAL VALUTA                  
008900*                                 ORDER VALUE, PREL CUSTOMER              
009000*                                 PRICE IN LOCAL CURRENCY                 
009100        05 4002-KDVALISO     PIC X(3).                                    
009200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009300*                                 CURRENCY CODE BY ISO-STANDARD.          
009400     03 4002-IDMSG3IV        PIC X(30).                                   
009500*                                 3IV MEDDELANDE-ID                       
009600*                                 3IV MESSAGE ID                          
009700     03 4002-IDSNO3IV        PIC X(25).                                   
009800*                                 SERIENR PÅ TERMINAL I 3IV               
009900*                                 SERIAL NO OF TERMINAL IN 3IV            
010000     03 4002-ADDISPXTRA      PIC X(20).                                   
010100*                                 ADDRESSTILLÄGG                          
010200*                                 ADDRESS EXTENTION                       
010300     03 4002-FILLER          PIC X(25).                                   
010400*** END OF VILMAII-COPY LENGTH= 1356 BYTES                                
