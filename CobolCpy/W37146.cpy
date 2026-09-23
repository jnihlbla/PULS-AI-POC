000100 01  POST.                                                                
000200*                                 COPYTEXT FÖR UPPDATERING AV             
000300*                                 BYTESBASEN EFTER KVITTNING.             
000400*                                                                         
000500     03 W37146.                                                           
000600        05 IDPTYP            PIC X(3).                                    
000700*                                 POSTTYP                                 
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 ORDER-INFO.                                                    
001300*                                                                         
001400           07 ORD-IDARTNR    PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600           07 ORD-TIAAMMDD-REG                                            
001700                             PIC S9(7)           COMP-3.                  
001800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001900           07 ORD-IDORDNR    PIC S9(5)           COMP-3.                  
002000*                                 ORDERNUMMER                             
002100        05 OBJEKT-INFO.                                                   
002200*                                                                         
002300           07 OBJ-IDARTNR    PIC S9(9)           COMP-3.                  
002400*                                 ARTIKELNUMMER                           
002500           07 OBJ-TIAAMMDD-REG                                            
002600                             PIC S9(7)           COMP-3.                  
002700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002800           07 OBJ-KDOBJEKT   PIC S9              COMP-3.                  
002900*                                 OBJEKTSKOD                              
003000           07 OBJ-IDORDNR    PIC S9(5)           COMP-3.                  
003100*                                 ORDERNUMMER                             
003200        05 AVB-KVITT-INFO.                                                
003300*                                                                         
003400           07 AVB-IDORDNR    PIC S9(5)           COMP-3.                  
003500*                                 ORDERNUMMER                             
003600           07 AVB-IDARTNR    PIC S9(9)           COMP-3.                  
003700*                                 ARTIKELNUMMER                           
003800           07 AVB-FLAVBOK    PIC S9              COMP-3.                  
003900*                                 AVBOKNINGSFLAGGA                        
004000*                                 1 = KVITTAD  0 = EJ KVITTAD             
004100*                                 0 = KVITTAD  1 = AVBOKAD                
004200           07 AVB-FLLIST     PIC S9              COMP-3.                  
004300*                                 ARTIKELN SKA LISTAS (1=JA)              
004400           07 AVB-IDKUNDRF   PIC X(10).                                   
004500*                                 KUNDENS REFERENS                        
004600        05 FAKTURA-INFO.                                                  
004700*                                                                         
004800           07 FAK-IDORDNR    PIC S9(5)           COMP-3.                  
004900*                                 ORDERNUMMER                             
005000           07 FAK-IDARTNR-OBJ                                             
005100                             PIC S9(9)           COMP-3.                  
005200*                                 ARTIKELNUMMER                           
005300           07 FAK-KDTDEB     PIC S9              COMP-3.                  
005400*                                 TILLÄGGSDEBITERING                      
005500           07 FAK-FLLIST     PIC S9              COMP-3.                  
005600*                                 ARTIKELN SKA LISTAS (1=JA)              
005700        05 ANTALS-INFO.                                                   
005800*                                                                         
005900           07 KVANTAL        PIC S9(7)           COMP-3.                  
006000*                                 ANTAL ALLMÄNT                           
006100*** END COPY W37146CCC0  LENGTH=69                                        
