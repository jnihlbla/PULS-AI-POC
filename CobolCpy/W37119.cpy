000100 01  W37119-CTX.                                                          
000200*                                 GODKÄNDA BYTESOBJEKT FRÅN BILD          
000300*                                 3171-3172                               
000400*                                 TILL VIPS.                              
000500     03 SOR0-IDDISTR         PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 SOR0-IDKUNDNR        PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 SOR0-IDRONR          PIC S9(7)           COMP-3.                  
001000*                                 RESTORDERNUMMER      IDRONR-002         
001100     03 SOR0-TIRODAT         PIC S9(7)           COMP-3.                  
001200*                                 RESTORDERDATUM         (ÅÅMMDD)         
001300     03 SOR0-IDPTYP          PIC X(3).                                    
001400*                                 POSTTYP                                 
001500     03 SOR0-IDLOPNR         PIC S9(5)           COMP-3.                  
001600*                                 LÖPNUMMER          IDLOPNR-002          
001700     03 IDPTYP               PIC X(3).                                    
001800*                                 POSTTYP                                 
001900     03 IDGMTREF.                                                         
002000*                                 GODSMOTTAGAREREFERENS                   
002100        05 IDDISTR           PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
002400*                                 KUNDNUMMER                              
002500        05 IDKUNDRF-GRP.                                                  
002600*                                 KUNDENS REFERENS (ORDERID)              
002700           07 FILLERX10      PIC X(10).                                   
002800           07 IDORDNR5-FILLER REDEFINES FILLERX10.                        
002900              09 IDORDNR5    PIC 9(5).                                    
003000*                                 ORDERNUMMER                             
003100              09 FILLER      PIC X(5).                                    
003200           07 IDORDNR7-FILLER REDEFINES FILLERX10.                        
003300              09 IDORDNR7    PIC 9(7).                                    
003400*                                 ORDERNUMMER                             
003500              09 FILLER      PIC X(3).                                    
003600     03 IDBYTRAP             PIC S9(7)           COMP-3.                  
003700*                                 RAPPORTNUMMER  BYTES                    
003800     03 TIREGDAT-GODK        PIC S9(7)           COMP-3.                  
003900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004000     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
004100*                                 OBJEKTNUMMER                            
004200     03 IDTABNR              PIC S9(3)           COMP-3.                  
004300*                                 TABELLNUMMER                            
004400     03 KVRETUR-GODK         PIC S9(5)           COMP-3.                  
004500*                                 ANTAL I RETUR                           
004600     03 KDBYTSTA-OBJ         PIC X.                                       
004700*                                 STATUSKOD BYTESOBJEKT                   
004800     03 KDBYTREF             PIC X(3).                                    
004900*                                 CENTRAL REFERENS                        
005000     03 IDBYTRAD             PIC 9(4).                                    
005100*                                 RADNUMMER                               
005200     03 IDDC                 PIC X(2).                                    
005300*                                 IDENTIFIERARE LAGER                     
005400     03 PRAVCOST-CORE        PIC S9(7)V9(2)      COMP-3.                  
005500*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005600*** END OF VILMAII-COPY LENGTH= 74 BYTES                                  
