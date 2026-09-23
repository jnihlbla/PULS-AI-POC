000100 01  POST.                                                                
000200*                                 SORTAREA FRÅN PROGRAM W3719000          
000300*                                           PLUS                          
000400*                                 WDA820-COPYTEXTEN                       
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
002000     03 WDA820-KEY.                                                       
002100        05 TIAAMMDD-REG      PIC S9(7)           COMP-3.                  
002200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002300        05 KDOBJEKT          PIC S9              COMP-3.                  
002400*                                 OBJEKTSKOD                              
002500        05 IDORDNR-KEY       PIC S9(5)           COMP-3.                  
002600*                                 ORDERNUMMER                             
002700     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
002800*                                 OBJEKTNUMMER                            
002900     03 IDORDNR              PIC S9(5)           COMP-3.                  
003000*                                 ORDERNUMMER                             
003100     03 KDCLAGER             PIC S9              COMP-3.                  
003200*                                 CENTRALLAGERKOD                         
003300     03 KVRETUR              PIC S9(7)           COMP-3.                  
003400*                                 ANTAL I RETUR                           
003500     03 KVRETUR-AVBOK        PIC S9(7)           COMP-3.                  
003600*                                 AVBOKAT ANT.RETUR                       
003700     03 TIAAMMDD-RENS        PIC S9(7)           COMP-3.                  
003800*                                 RENSNINGSDATUM                          
003900     03 FLRO                 PIC S9              COMP-3.                  
004000*                                 RESTOTERAD ARTIKEL ? (1=JA)             
004100     03 KDBORT               PIC S9              COMP-3.                  
004200*                                 BORTTAGSKOD I BYTES                     
004300*                                 0 = INGET BORTTAG                       
004400*                                 1 = MASKINELLT BORTTAG                  
004500*                                 2 = MANUELLT BORTTAG                    
004600*                                 3 = MAN. BORTTAG SALDO UPPD.            
004700*                                     GÄLLER ENDAST OBJEKT-SEGM.          
004800*                                 9 = MAN. BORTTAG EJ SALDO UPPD.         
004900*                                     GÄLLER ENDAST OBJEKT-SEGM.          
005000     03 FLJUST               PIC S9              COMP-3.                  
005100*                                 JUSTERINGSFLAGGA                        
005200*                                 0 = NEJ  1 = DATUM JUSTERAT             
005300     03 FLLIST               PIC S9              COMP-3.                  
005400*                                 ARTIKELN SKA LISTAS (1=JA)              
005500     03 IDKUNDRF             PIC X(10).                                   
005600*                                 KUNDENS REFERENS (ORDERID)              
005700*** END COPY W3719520    LENGTH=66                                        
