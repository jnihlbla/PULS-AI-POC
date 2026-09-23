000100 01  ORD-WDB414.                                                          
000200*                                 KUNDREGISTER FÖR DÖSKALLAR              
000300*                                 ORDERHUVUDSEGMENT                       
000400*                                 FYSISK NYCKEL: WDB414KY                 
000500*                                  (KDFRAKT, KDORDKL)                     
000600     03 ORD-KDFRAKT          PIC S9(3)           COMP-3.                  
000700*                                 FRAKTSÄTT DC TILL KUND                  
000800*                                 FREIGHT CODE                            
000900     03 ORD-KDORDKL          PIC S9              COMP-3.                  
001000*                                 ORDERKLASS                              
001100*                                 ORDER CLASS                             
001200     03 ORD-BEKUNDRF         PIC X(10).                                   
001300*                                 KUNDENS REFERENS                        
001400*                                 CUSTOMERS REFERENCE                     
001500     03 ORD-BEVARREF         PIC X(10).                                   
001600*                                 VÅR REFERENS                            
001700*                                 OUR REFERENCE                           
001800     03 ORD-IDANALYS         PIC X(12).                                   
001900*                                 ANALYSNUMMER                            
002000*                                 ANALYSIS NUMBER                         
002100     03 ORD-IDFTG            PIC 9(2).                                    
002200*                                 FÖRETAGSID EKONOM REDOVISNING           
002300*                                 COMPANY IDENTITY ACCOUNTING             
002400     03 ORD-IDKONTO          PIC S9(11)          COMP-3.                  
002500*                                 KONTO                                   
002600*                                 ACCOUNT                                 
002700     03 ORD-IDKST            PIC X(10).                                   
002800*                                 KOSTNADSSTÄLLE                          
002900*                                 COST CENTRE                             
003000     03 ORD-IDSKYLT          PIC X(3).                                    
003100*                                 NATIONALITETSTECKEN                     
003200*                                 SPRÅKIDENTIFIKATION                     
003300*                                 NATIONALITY SIGN                        
003400*                                 LANGUAGE IDENTIFIER                     
003500     03 ORD-KDFAKTYP         PIC X.                                       
003600*                                 FAKTURATYP                              
003700*                                 INVOICE TYPE                            
003800     03 ORD-KDNOTES          PIC X(2).                                    
003900*                                 NOTERINGSKOD                            
004000*                                 CODE FOR NOTES                          
004100     03 ORD-KDROPACK         OCCURS 2 TIMES                               
004200                             PIC X.                                       
004300*                                 FRISLÄPPNINGSKOD RO/DO                  
004400*                                 CONSOLIDATION BO/DO                     
004500     03 ORD-TID              OCCURS 10 TIMES                              
004600                             PIC S9              COMP-3.                  
004700*                                 DAGNUMMER I VECKA (MÅNDAG = 1)          
004800*                                 DAY NO. IN WEEK   (MONDAY = 1)          
004900     03 ORD-TISTADAT         PIC S9(7)           COMP-3.                  
005000*                                 GENERELLT STARTDATUM                    
005100*                                 GENERAL START DATE                      
005200*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  
