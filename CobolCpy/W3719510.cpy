000100 01  POST.                                                                
000200*                                 SORTAREA FRÅN PROGRAM W3719000          
000300*                                           PLUS                          
000400*                                 WDA810-COPYTEXTEN                       
000500*                                                                         
000600     03 IDDISTR-SORT         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR-SORT        PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDARTNR-SORT         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 TIAAMMDD-REG-SORT    PIC S9(7)           COMP-3.                  
001300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001400     03 KDOBJEKT-SORT        PIC S9              COMP-3.                  
001500*                                 OBJEKTSKOD                              
001600     03 IDORDNR-SORT         PIC S9(5)           COMP-3.                  
001700*                                 ORDERNUMMER                             
001800     03 IDPTYP-SORT          PIC X(3).                                    
001900*                                 POSTTYP                                 
002000     03 WDA810-KEY.                                                       
002100        05 TIAAMMDD-REG      PIC S9(7)           COMP-3.                  
002200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002300        05 IDORDNR           PIC S9(5)           COMP-3.                  
002400*                                 ORDERNUMMER                             
002500     03 KDCLAGER             PIC S9              COMP-3.                  
002600*                                 CENTRALLAGERKOD                         
002700     03 KVBEART              PIC S9(7)           COMP-3.                  
002800*                                 BESTÄLLT ANTAL STYCKEN                  
002900     03 KVANTAL-FAKT         PIC S9(7)           COMP-3.                  
003000*                                 FAKTURERAT ANTAL                        
003100*                                                                         
003200     03 KVROS                PIC S9(7)           COMP-3.                  
003300*                                 RESTORDERSALDO                          
003400     03 KVAVBART             PIC S9(7)           COMP-3.                  
003500*                                 AVBOKAT ANTAL ARTIKLAR                  
003600     03 KVLEVANM             PIC S9(7)           COMP-3.                  
003700*                                 LEVERANSANMÄRKNINGSANTAL                
003800     03 TIAAMMDD-FAKT        PIC S9(7)           COMP-3.                  
003900*                                 SENASTE FAKTURERINGSDATUM               
004000     03 TIAAMMDD-TDEB        PIC S9(7)           COMP-3.                  
004100*                                 TILLÄGGSDEBITERINGS-DATUM               
004200     03 KDBORT               PIC S9              COMP-3.                  
004300*                                 BORTTAGSKOD I BYTES                     
004400*                                 0 = INGET BORTTAG                       
004500*                                 1 = MASKINELLT BORTTAG                  
004600*                                 2 = MANUELLT BORTTAG                    
004700*                                 3 = MAN. BORTTAG SALDO UPPD.            
004800*                                     GÄLLER ENDAST OBJEKT-SEGM.          
004900*                                 9 = MAN. BORTTAG EJ SALDO UPPD.         
005000*                                     GÄLLER ENDAST OBJEKT-SEGM.          
005100     03 FLJUST               PIC S9              COMP-3.                  
005200*                                 JUSTERINGSFLAGGA                        
005300*                                 0 = NEJ  1 = DATUM JUSTERAT             
005400*** END COPY W3719510    LENGTH=61                                        
