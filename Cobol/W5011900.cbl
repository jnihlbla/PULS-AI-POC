000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5011900.                                                
000400 AUTHOR.         GUN LÖFGREN.                                             
000500 DATE-WRITTEN.   NOVEMBER 1996.                                           
000600*                                                                         
000700*    REMARKS.                                                             
000800*        IMSDC UPPDATERINGSPROGRAM FÖR EKONOMI.                           
000900*                                                                         
001000*        UPPDATERINGEN SKER PÅ TVÅ SÄTT:                                  
001100*        - FRÅN INKÖP GENOM TRANS W5T119X FRÅN W553XX                     
001200*          VIA WLKOMA (WDP8), KOMMUNIKATIONSDATABAS.                      
001300*        - FRÅN SKÄRMEN                                                   
001400*                                                                         
001500*        OM UPPDATERINGEN KOM FRÅN INKÖP OCH                              
001600*        UPPDATERINGEN GICK BRA SKICKAS OK-MEDDELANDE                     
001700*        TILL DISPATCHER ANNARS SKICKAS FELMEDDELANDE.                    
001800*                                                                         
001900*                                                                         
002000*        PROGRAMMET LÄSER WLARTC (WDK6) OCH LÄGGER UPP NYA                
002100*        BESTÄLLNINGSPRISER ELLER ÄNDRAR BEFINTLIGA PÅ                    
002200*        WLARTC21 (WDK621) FÖR ICKE HUVUDLEVERANTÖRER.                    
002300*                                                                         
002400*        PROGRAMMET RENSAR AUTOMATISKT DEN 6:E RADEN (21SEGM)             
002500*        OM DET BLIR FLER ÄN 5 RADER PÅ SAMMA LEVERANTÖR SOM              
002600*        ÄR FLAGGAD ICKE HUVUDLEVERANTÖR, VID NYUPPLÄGG AV                
002700*        NY PRISRAD.                                                      
002800*                                                                         
002900*    INDATA.                                                              
003000*        TRANSAKTION: W5T119                                              
003100*                     W5T119U                                             
003200*                     W5T119X                                             
003300*                                                                         
003400*        MID:         W5I11901                                            
003500*                     W5I11901 + WMSGKOM                                  
003600*                                                                         
003700*    UTDATA.                                                              
003800*        MOD:         W5O11901                                            
003900     EJECT                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100     SKIP3                                                                
004200 DATA DIVISION.                                                           
004300                                                                          
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600*    -COPY WY2000W9                                                       
004700     SKIP3                                                                
004800*    -COPY WY2000W1                                                       
004900     SKIP3                                                                
005000*01       -COPY WWDCKONS                                                  
005100*01       -COPY WWDC99                                                    
005200 77  IDPGM                   PIC X(8)       VALUE 'W5011900'.             
005300 77  JA                      PIC X          VALUE 'J'.                    
005400 77  NEJ                     PIC X          VALUE 'N'.                    
005500                                                                          
005600 77  NYCKLAR-OK              PIC X          VALUE 'J'.                    
005700 77  FLFEL-FAELT             PIC X          VALUE 'N'.                    
005800 77  FLSLUTA-LAS             PIC X          VALUE 'N'.                    
005900 77  FOERSTA-POST            PIC X          VALUE 'J'.                    
006000 77  PRIS-FINNS              PIC X          VALUE ' '.                    
006100 77  LOKAL-PRODUKT           PIC X          VALUE 'N'.                    
006200 77  FIRST-INLEV             PIC X          VALUE 'N'.                    
006300 77  W-DATE-AAMM             PIC 9(4)       VALUE ZERO.                   
006400     EJECT                                                                
006500 01  SUBPGM.                                                              
006600     03  CBLTDLI             PIC X(8)       VALUE 'CBLTDLI '.             
006700     03  FELLOG              PIC X(8)       VALUE 'FELLOG  '.             
006800     03  WDATKONV            PIC X(8)       VALUE 'WDATKONV'.             
006900     03  WDECSTR             PIC X(8)       VALUE 'WDECSTR '.             
007000     03  W005INIT            PIC X(8)       VALUE 'W005INIT'.             
007100     03  W006KOM             PIC X(8)       VALUE 'W006KOM '.             
007200     03  WMEDKONV            PIC X(8)       VALUE 'WMEDKONV'.             
007300     03  W510CURR            PIC X(8)       VALUE 'W510CURR'.             
007400                                                                          
007500 01  W-IDTRANS               PIC X(4)       VALUE SPACE.                  
007600     88 EGEN-TRANS                          VALUE '5119'.                 
007700     88 GODK-TRANS                          VALUE '5119'.                 
007800                                                                          
007900 01  DIVERSE.                                                             
008000   03  INDX                  PIC S9(9)      VALUE +0 COMP SYNC.           
008100   03  WS-IDLEVNR            PIC X(5)       VALUE SPACE.                  
008200   03  WS-KDPRBEH            PIC X(1)       VALUE SPACE.                  
008300                                                                          
008400   03  IDARTNR-WS            PIC X(9)       VALUE SPACE.                  
008500   03  IDLEVNR-WS            PIC X(5)       VALUE SPACE.                  
008600   03  WS-TIUPPDAT           PIC S9(7)     COMP-3.                        
008700   03  WS-TIUPPTID           PIC S9(9)     COMP-3.                        
008800                                                                          
008900   03  W-DAPRLIST-AKT        PIC 9(8).                                    
009000   03  FILLER  REDEFINES W-DAPRLIST-AKT.                                  
009100    05 W-SEKEL               PIC 9(2).                                    
009200    05 W-AAR                 PIC 9(2).                                    
009300    05 FILLER                PIC 9(4).                                    
009400                                                                          
009500   03  W-DAPRLIST-MAX        PIC 9(8)       VALUE 99999999.               
009600   03  W-KDERS               PIC S9(3)      VALUE ZERO   COMP-3.          
009700                                                                          
009800   03  FILLER                PIC X(16)      VALUE 'WS-ACKAR'.             
009900   03  W-ANT-EJ-HUVLEV-RADER PIC S9(3)      VALUE +0.                     
010000   03  W-PRARTBEL            PIC S9(8)V9(5) VALUE +0.                     
010100   03  W-PRARTBES            PIC S9(7)V9(2) VALUE +0     COMP-3.          
010200   03  W-PRARTSJK            PIC S9(7)V9(2) VALUE +0     COMP-3.          
010300   03  W-PRINK               PIC S9(7)V9(2) VALUE +0     COMP-3.          
010400   03  W-PRARTSTD            PIC S9(7)V9(2) VALUE +0     COMP-3.          
010500   03  W-PRKURS              PIC S9(5)V9(2) VALUE +0     COMP-3.          
010600   03  W-RETULF              PIC S9(3)V9(4) VALUE +0.                     
010700   03  W-REVALUTA            PIC S9(5)      VALUE +0     COMP-3.          
010800   03  WS-LS-CDC             PIC S9(9)      VALUE +0.                     
010900   03  WS-LS-SDC             PIC S9(9)      VALUE +0.                     
011000   03  W-KDVALISO            PIC X(3)       VALUE SPACE.                  
011100   03  W-FLPRFIL             PIC X          VALUE 'N'.                    
011200                                                                          
011300   03  W-IDSTRDATA-X.                                                     
011400     05  W-IDSTRDATA-N       PIC 9(8)V9(5).                               
011500     05  FILLER              PIC XX.                                      
011600                                                                          
011700   03  DAGENS-DAT.                                                        
011800     05  DAGENS-SEKEL        PIC 9(2).                                    
011900     05  DAGENS-DATUM        PIC 9(6).                                    
012000     05  FILLER  REDEFINES DAGENS-DATUM.                                  
012100      07 DAGENS-AAR          PIC 9(2).                                    
012200      07 FILLER              PIC 9(4).                                    
012300   03  DAGENS-TID            PIC 9(8).                                    
012400   03  FILLER  REDEFINES DAGENS-TID.                                      
012500     05  DAGENS-KLOCK        PIC 9(6).                                    
012600     05  FILLER              PIC 9(2).                                    
012700                                                                          
012800   03  DAGENS-AAAAMMDD       PIC 9(8) VALUE ZERO.                         
012900                                                                          
013000   03  WS-SEKTION            PIC X(40)   VALUE SPACE.                     
013100                                                                          
013200   03  IDINLEV-FAELT.                                                     
013300     05  IDINLEV-NYCKEL      PIC S9(15)  VALUE ZERO.                      
013400     05  IDINLEV-MAX         PIC S9(15)  VALUE +999999999999999.          
013500                                                                          
013600     EJECT                                                                
013700                                                                          
013800*01  -COPY WWPRODSL                                                       
013900                                                                          
014000 01  FILLER                  PIC X(16)   VALUE 'DLI-NYCKLAR'.             
014100 01  NYCKLAR-TILL-DLI.                                                    
014200     03  W-IDARTNR-X.                                                     
014300         05  W-IDARTNR       PIC S9(9)   VALUE +0  COMP-3.                
014400     03  W-IDLEVNR-X.                                                     
014500         05  W-IDLEVNR       PIC X(5)    VALUE SPACE.                     
014600     03  W-IDLANDX2-X.                                                    
014700         05  W-IDLANDX2      PIC X(2)    VALUE SPACE.                     
014800     03  W-IDLAND-X.                                                      
014900         05  W-IDLAND        PIC X(2)    VALUE SPACE.                     
015000     03  W-WDK621KY-X.                                                    
015100         05  W-DAPRLIST-21   PIC 9(8)    VALUE 0.                         
015200         05  W-IDLEVNR-21    PIC X(5)    VALUE LOW-VALUE.                 
015300     03  W-KDNOTTYP-X.                                                    
015400         05    W-KDNOTTYP    PIC S9(1)   VALUE +8  COMP-3.                
015500     03  W-IDSKYLT-X.                                                     
015600         05    W-IDSKYLT     PIC X(3)    VALUE 'GB '.                     
015700     03  W-IDDC-B6-X.                                                     
015800         05 W-IDDC-B6        PIC X(2)    VALUE SPACE.                     
015900     EJECT                                                                
016000     SKIP3                                                                
016100 01  MEDDELANDE.                                                          
016200     03  W-FEL-1               PIC X(40)   VALUE                          
016300             'PRESS PF11 KEY TO UPDATE             '.                     
016400                                                                          
016500     EJECT                                                                
016600 01  MESSAGE-CODES.                                                       
016700     03  ERR-WRONG-DATA          PIC X(3)    VALUE '001'.                 
016800     03  ERR-UPDATE-NOT-PF11     PIC X(3)    VALUE '003'.                 
016900     03  ERR-NO-UPDATE           PIC X(3)    VALUE '007'.                 
017000     03  ERR-PF11-NO-DATA        PIC X(3)    VALUE '011'.                 
017100     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
017200     03  ERR-PART-SUPERSEDED     PIC X(3)    VALUE '018'.                 
017300     03  ERR-NOT-NUMERIC         PIC X(3)    VALUE '020'.                 
017400     03  ERR-VENDOR-MISSING      PIC X(3)    VALUE '092'.                 
017500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
017600     03  ERR-KDVALISO-MISSING    PIC X(3)    VALUE '148'.                 
017700     03  ERR-PART-EXPIRE         PIC X(3)    VALUE '258'.                 
017800     03  ERR-INFO-MISSING        PIC X(3)    VALUE '303'.                 
017900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
018000     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
018100     EJECT                                                                
018200*                   ****    PARAMETRAR TILL WMEDKONV                      
018300 01  FILLER                    PIC X(16)   VALUE 'WMEDAREA'.              
018400*01  -COPY WMEDAREA                                                       
018500     EJECT                                                                
018600*                   ****    PARAMETRAR TILL W005INIT                      
018700 01  FILLER                    PIC X(16)   VALUE 'WMSGINIT'.              
018800*01  -COPY WMSGINIT                                                       
018900     EJECT                                                                
019000 01  WDATAREA                  PIC X(8)    VALUE 'WDATAREA'.              
019100     SKIP3                                                                
019200*01       -COPY WDATAREA                                                  
019300     EJECT                                                                
019400 01  FILLER                    PIC X(8)    VALUE 'WDECSTR'.               
019500     SKIP3                                                                
019600*01       -COPY WDECSTR                                                   
019700*01  -COPY W510CURR                                                       
019800     EJECT                                                                
019900******************************************************************        
020000*                                                                         
020100*                AREOR FOR MFS OCH SKÄRMHANTERING                         
020200*                                                                         
020300******************************************************************        
020400 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
020500     SKIP3                                                                
020600*01  MID -COPY W5I11901 -PRE MID-                                         
020700     EJECT                                                                
020800*01  -COPY WMSGAREA                                                       
020900     EJECT                                                                
021000*  03  MOD -COPY W5O11901 -PRE MOD- -RED MSG-AREA                         
021100     EJECT                                                                
021200*01  -COPY WMFSAREA                                                       
021300******************************************************************        
021400*                                                                         
021500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021600*                                                                         
021700******************************************************************        
021800                                                                          
021900 01  IMS-WS.                                                              
022000     03    FILLER              PIC X(16)   VALUE 'IMS-WS     '.           
022100     SKIP3                                                                
022200*                        **** STATUS-KOD FRÅN IMS ****                    
022300     03    STATUS-WS           PIC XX.                                    
022400         88    SEGMENT-FINNS               VALUE '  '.                    
022500         88    SEGMENT-SAKNAS              VALUE 'GE'.                    
022600         88    SEGMENT-FINNS-REDAN         VALUE 'II'.                    
022700     SKIP3                                                                
022800     03    GODK-STATUSKODER.                                              
022900         05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.          
023000     SKIP3                                                                
023100 01  SSA1                      PIC X(64).                                 
023200 01  SSA2                      PIC X(64).                                 
023300 01  SSA3                      PIC X(64).                                 
023400     EJECT                                                                
023500*                        **** IMS FUNKTIONSKODER ****                     
023600*01    -COPY W0003                                                        
023700     EJECT                                                                
023800*                            DLI INPUT-OUTPUT AREA                        
023900 01  FILLER                 PIC X(16)  VALUE 'WLARTC01'.                  
024000*                                                                         
024100*01  WLARTC01 -COPY WDK601                                                
024200     EJECT                                                                
024300 01  FILLER                 PIC X(16)  VALUE 'WLARTC11'.                  
024400*                                                                         
024500*01  WLARTC11 -COPY WDK611                                                
024600     EJECT                                                                
024700 01  FILLER                 PIC X(16)  VALUE 'WLARTC21'.                  
024800*                                                                         
024900*01  WLARTC21 -COPY WDK621                                                
025000     EJECT                                                                
025100 01  FILLER                 PIC X(16)  VALUE 'WLARTS01'.                  
025200*                                                                         
025300*01  WLARTS01   -COPY WDK701                                              
025400     EJECT                                                                
025500 01  FILLER                 PIC X(16)  VALUE 'WLARTS11'.                  
025600*                                                                         
025700*01  WLARTS11   -COPY WDK711                                              
025800     EJECT                                                                
025900 01  FILLER                 PIC X(16)  VALUE 'WLBENA11'.                  
026000*                                                                         
026100*01  WLBENA11 -COPY WDD311 -PRE BEN-                                      
026200     EJECT                                                                
026300 01  FILLER                 PIC X(16)  VALUE 'WDK712'.                    
026400*01  WDK712   -COPY WDK712                                                
026500     EJECT                                                                
026600                                                                          
026700 01  FILLER                 PIC X(16)  VALUE 'WLLEVA01'.                  
026800*                                                                         
026900*01  WLLEVA01 -COPY WDF101                                                
027000     EJECT                                                                
027100 01  FILLER                 PIC X(16)  VALUE 'WLLEVA11'.                  
027200*                                                                         
027300*01  WLLEVA11 -COPY WDF102  -PRE LEV-                                     
027400     EJECT                                                                
027500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
027600 01   DLI-IO-AREA-B601.                                                   
027700*     03  -COPY WDB601                                                    
027800                                                                          
027900     EJECT                                                                
028000*    --- AREA FÖR KOMMUNIKATION MED DISPATCHER                            
028100*                                                                         
028200 01  FILLER                 PIC X(16)   VALUE 'KOM-DISP-IO-AREA'.         
028300     SKIP3                                                                
028400 01  KOM-IO-AREA.                                                         
028500*    03  -COPY WMSGKOM                                                    
028600     EJECT                                                                
028700 LINKAGE SECTION.                                                         
028800*01    -COPY W0009     -PRE MSG-                                          
028900                                                                          
029000*01    -COPY W0009     -PRE ALT-                                          
029100     EJECT                                                                
029200*01  -COPY W0008     -PRE USEA-                                           
029300         05  FILLER           PIC X.                                      
029400                                                                          
029500*01    -COPY W0008     -PRE ARTC-                                         
029600     05  FILLER                  PIC X(13).                               
029700     EJECT                                                                
029800*01    -COPY W0008     -PRE ARTS-                                         
029900     05  FILLER                  PIC X(13).                               
030000                                                                          
030100*01    -COPY W0008     -PRE WDK712-                                       
030200     05  FILLER                  PIC X(13).                               
030300                                                                          
030400*01    -COPY W0008     -PRE BEN-                                          
030500     05  FILLER                  PIC X(8).                                
030600     EJECT                                                                
030700*01    -COPY W0008     -PRE LEV-                                          
030800     05  FILLER                  PIC X(5).                                
030900                                                                          
031000*01    -COPY W0008     -PRE 9305-                                         
031100     05  FILLER                  PIC X(30).                               
031200                                                                          
031300*01  -COPY W0008       -PRE WDB6-                                         
031400     05  FILLER                  PIC X.                                   
031500                                                                          
031600     EJECT                                                                
031700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB                       
031800                           ARTC-PCB ARTS-PCB WDK712-PCB BEN-PCB           
031900                           LEV-PCB  9305-PCB WDB6-PCB.                    
032000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
032100                           ARTC-PCB ARTS-PCB  WDK712-PCB BEN-PCB          
032200                           LEV-PCB  9305-PCB WDB6-PCB.                    
032300                                                                          
032400     PERFORM IMS-GET-MSG                                                  
032500     IF SEGMENT-FINNS                                                     
032600       PERFORM A-INIT                                                     
032700       PERFORM B-KOLLA-NYCKEL                                             
032800       IF MFS-UPD-X                                                       
032900         PERFORM IMS-GN-MSG-KOM                                           
033000       END-IF                                                             
033100       IF NYCKLAR-OK = JA                                                 
033200         IF MFS-UPDATE OR MFS-UPD-X                                       
033300           PERFORM C-UPPDATERA                                            
033400         ELSE                                                             
033500*          HÄR BÖRJAR SÖKNING                     **                      
033600           IF MID-IDARTNR-IN = ALL '+'                                    
033700           AND MID-KDPRURSP-U NOT = ALL '+'                               
033800           AND MID-TIPRLIST-U NOT = ALL '+'                               
033900           AND MID-PRARTBEL-U NOT = ALL '+'                               
034000           AND MID-KDVALISO-U NOT = ALL '+'                               
034100           AND EGEN-TRANS                                                 
034200              MOVE W-FEL-1 TO MOD-TEMFSFEL                                
034300              PERFORM MFS-ROER-EJ-FAELT-UTDATA                            
034400              PERFORM MFS-ROER-EJ-FAELT-INDATA                            
034500              MOVE MFS-ADD-LAES-IN-FAELT TO                               
034600                   MOD-KDPRURSP-ATTR                                      
034700                   MOD-TIPRLIST-ATTR                                      
034800                   MOD-PRARTBEL-ATTR                                      
034900                   MOD-KDVALISO-ATTR                                      
035000                   MOD-IDDC-ATTR                                          
035100           ELSE                                                           
035200              PERFORM D-SOEKNING                                          
035300           END-IF                                                         
035400         END-IF                                                           
035500       ELSE                                                               
035600         MOVE 'GB'              TO MED-IDSKYLT                            
035700         MOVE ERR-NOT-NUMERIC   TO MED-IDMFSFEL                           
035800         CALL WMEDKONV USING MED-WMEDAREA                                 
035900         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
036000         PERFORM MFS-RENSA-FAELT-UTDATA                                   
036100         PERFORM MFS-RENSA-FAELT-INDATA                                   
036200       END-IF                                                             
036300       IF MFS-UPD-X                                                       
036400         IF MSG-KOM-IDMFSMED = SPACE                                      
036500           MOVE INF-UPDATE-DONE TO MSG-KOM-IDMFSMED                       
036600         END-IF                                                           
036700         MOVE SPACE             TO MSG-KOM-KDSVAR                         
036800         PERFORM IMS-INSERT-MSG-KOM                                       
036900       ELSE                                                               
037000         COMPUTE  MSG-KVLL  = LENGTH OF MOD-W5O11901 + 4                  
037100         PERFORM IMS-INSERT-MSG                                           
037200       END-IF                                                             
037300     END-IF                                                               
037400                                                                          
037500     MOVE ZERO TO RETURN-CODE                                             
037600     GOBACK                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 A-INIT SECTION.                                                          
038000                                                                          
038100     MOVE 'A-INIT'   TO WS-SEKTION                                        
038200                                                                          
038300     ACCEPT DAGENS-DATUM FROM DATE                                        
038400     IF DAGENS-AAR < 50                                                   
038500       MOVE 20     TO DAGENS-SEKEL                                        
038600     ELSE                                                                 
038700       MOVE 19     TO DAGENS-SEKEL                                        
038800     END-IF                                                               
038900                                                                          
039000     ACCEPT DAGENS-TID    FROM TIME                                       
039100                                                                          
039200     IF MSG-DUBBLA-TRANSKODER                                             
039300        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I11901                
039400        MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                          
039500        MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                         
039600     ELSE                                                                 
039700        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I11901                 
039800        MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                          
039900        MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                         
040000     END-IF                                                               
040100                                                                          
040200     MOVE MSG-KDTRTYP             TO MFS-KDTRTYP                          
040300     MOVE MFS-IDTRANS             TO W-IDTRANS                            
040400                                                                          
040500     MOVE LOW-VALUE       TO MSG-AREA                                     
040600     MOVE 'W5O119N1'      TO MFS-IDMOD                                    
040700     MOVE '5119'          TO MOD-IDTRANS                                  
040800                                                                          
040900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
041000                             MOD-IDLEVNR-IN                               
041100                             MOD-TEMFSFEL                                 
041200                             MOD-TEMFSINF                                 
041300     PERFORM MFS-RENSA-FAELT-INDATA                                       
041400                                                                          
041500     .                                                                    
041600     EJECT                                                                
041700 B-KOLLA-NYCKEL SECTION.                                                  
041800                                                                          
041900     MOVE 'B-KOLLA-NYCKEL'    TO WS-SEKTION.                              
042000                                                                          
042100     IF MFS-UPD-X                                                         
042200****************  DISPATCHANROP                                           
042300                                                                          
042400       IF MID-IDARTNR-IN = ALL '+'                                        
042500         MOVE ZERO           TO IDARTNR-WS                                
042600         INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO               
042700       ELSE                                                               
042800         MOVE MID-IDARTNR-IN TO IDARTNR-WS                                
042900       END-IF                                                             
043000       IF MID-IDLEVNR-IN = ALL '+'                                        
043100         MOVE SPACE          TO IDLEVNR-WS                                
043200       ELSE                                                               
043300         MOVE MID-IDLEVNR-IN TO IDLEVNR-WS                                
043400       END-IF                                                             
043500     ELSE                                                                 
043600       MOVE ALL '+'           TO MSGI-WMSGINIT                            
043700       MOVE '001'             TO MSGI-KDCALL                              
043800       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
043900       MOVE '5119'            TO MSGI-IDTRANS                             
044000       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
044100       IF EGEN-TRANS                                                      
044200         MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                             
044300         MOVE MID-IDLEVNR-IN  TO MSGI-IDLEVNR                             
044400       ELSE                                                               
044500           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
044600           MOVE MID-IDLEVNR-IN TO MSGI-IDLEVNR                            
044700           MOVE MSGI-IDLEVNR      TO IDLEVNR-WS                           
044800       END-IF                                                             
044900       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
045000                                                                          
045100       IF MSGI-IDLAND-SPR NOT = 'GB'                                      
045200          MOVE 'S  ' TO W-IDSKYLT                                         
045300          MOVE 'S  ' TO MED-IDSKYLT                                       
045400       ELSE                                                               
045500          MOVE 'GB ' TO W-IDSKYLT                                         
045600          MOVE 'GB ' TO MED-IDSKYLT                                       
045700       END-IF                                                             
045800                                                                          
045900       MOVE MSGI-IDARTNR      TO IDARTNR-WS                               
046000       INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                     
046100       IF EGEN-TRANS                                                      
046200         MOVE MSGI-IDLEVNR      TO IDLEVNR-WS                             
046300       END-IF                                                             
046400     END-IF                                                               
046500                                                                          
046600     MOVE MID-IDDC-U       TO W-IDDC-B6                                   
046700                              WS-IDDC                                     
046800                                                                          
046900     IF NOT EGEN-TRANS                                                    
047000       MOVE ' '               TO MFS-KDTRTYP                              
047100     END-IF                                                               
047200                                                                          
047300     IF IDARTNR-WS NOT NUMERIC                                            
047400       MOVE NEJ               TO NYCKLAR-OK                               
047500     ELSE                                                                 
047600       MOVE IDARTNR-WS        TO W-IDARTNR                                
047700                                 MOD-IDARTNR-UT                           
047800       INSPECT MOD-IDARTNR-UT REPLACING                                   
047900               LEADING ZERO BY SPACE                                      
048000     END-IF                                                               
048100                                                                          
048200       MOVE IDLEVNR-WS        TO W-IDLEVNR                                
048300                                 WS-IDLEVNR                               
048400       MOVE WS-IDLEVNR        TO MOD-IDLEVNR-UT                           
048500     IF NYCKLAR-OK = JA                                                   
048600       MOVE IDARTNR-WS        TO MOD-IDARTNR-UT                           
048700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
048800       MOVE WS-IDLEVNR        TO MOD-IDLEVNR-UT                           
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200 C-UPPDATERA  SECTION.                                                    
049300     MOVE 'C-UPPDATERA'  TO WS-SEKTION                                    
049400                                                                          
049500     PERFORM CA-FORMELL-KONTROLL-MID                                      
049600     IF MFS-UPDATE                                                        
049700       IF FLFEL-FAELT = NEJ                                               
049800         IF MID-TIPRLIST-U NOT = ALL '+'                                  
049900           PERFORM CB-KONTROLLERA-MID-MOT-BAS                             
050000         END-IF                                                           
050100       END-IF                                                             
050200     END-IF                                                               
050300                                                                          
050400     PERFORM MFS-ROER-EJ-FAELT-UTDATA                                     
050500     IF FLFEL-FAELT = JA                                                  
050600       PERFORM MFS-ROER-EJ-FAELT-INDATA                                   
050700     ELSE                                                                 
050800       PERFORM MFS-RENSA-FAELT-INDATA                                     
050900       PERFORM MFS-FORMATETS-ATTR-INDATA                                  
051000       PERFORM CE-UPPDATERA                                               
051100       IF FLFEL-FAELT = JA                                                
051200         PERFORM MFS-ROER-EJ-FAELT-INDATA                                 
051300       ELSE                                                               
051400         MOVE 'GB'             TO MED-IDSKYLT                             
051500         MOVE INF-UPDATE-DONE  TO MED-IDMFSFEL                            
051600         CALL WMEDKONV USING MED-WMEDAREA                                 
051700         MOVE MED-MFSFEL       TO MOD-TEMFSINF                            
051800         PERFORM MFS-OEPPNA-FAELT-INDATA                                  
051900       END-IF                                                             
052000     END-IF                                                               
052100                                                                          
052200     .                                                                    
052300     EJECT                                                                
052400 CA-FORMELL-KONTROLL-MID SECTION.                                         
052500                                                                          
052600     MOVE 'CA-FORMELL-KONTROLL-MID'  TO WS-SEKTION                        
052700                                                                          
052800     PERFORM IMS-GU-WLARTC01                                              
052900                                                                          
053000     IF MFS-UPD-X                                                         
053100       IF SEGMENT-SAKNAS                                                  
053200         MOVE 'GB'             TO MED-IDSKYLT                             
053300         MOVE ERR-WRONG-KEY TO MSG-KOM-IDMFSMED                           
053400         MOVE '5'           TO MSG-KOM-KDSVAR                             
053500         CALL WMEDKONV USING MED-WMEDAREA                                 
053600         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
053700         MOVE JA            TO FLFEL-FAELT                                
053800       END-IF                                                             
053900     ELSE                                                                 
054000       IF SEGMENT-FINNS                                                   
054100         IF MID-KDPRURSP-U = ALL '+'  AND                                 
054200            MID-TIPRLIST-U = ALL '+'  AND                                 
054300            MID-PRARTBEL-U = ALL '+'  AND                                 
054400            MID-KDVALISO-U = ALL '+'  AND                                 
054500            MID-IDDC-U     = ALL '+'                                      
054600           MOVE 'GB'             TO MED-IDSKYLT                           
054700           MOVE ERR-PF11-NO-DATA TO MED-IDMFSFEL                          
054800           CALL WMEDKONV USING MED-WMEDAREA                               
054900           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
055000           MOVE JA          TO FLFEL-FAELT                                
055100         ELSE                                                             
055200* **          HÄR KONTROLLERAS PRISHÄRSTAMNING               **           
055300                                                                          
055400           IF MID-KDPRURSP-U = 'F' OR 'B' OR 'A' OR 'P' OR 'Å'            
055500                               OR 'M' OR 'I' OR ' ' OR '+'                
055600             MOVE MFS-ALFA-FAELT-RAETT TO                                 
055700                    MOD-KDPRURSP-ATTR                                     
055800           ELSE                                                           
055900             MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDPRURSP-ATTR                
056000             MOVE 'GB'              TO MED-IDSKYLT                        
056100             MOVE ERR-WRONG-DATA    TO MED-IDMFSFEL                       
056200             CALL WMEDKONV USING MED-WMEDAREA                             
056300             MOVE MED-MFSFEL        TO MOD-TEMFSFEL                       
056400             MOVE JA                TO FLFEL-FAELT                        
056500           END-IF                                                         
056600                                                                          
056700* **          HÄR KONTROLLERAS OM ANGIVIT DATUM ÄR ETT       **           
056800* **          GILTIGT DATUM                                  **           
056900                                                                          
057000           IF MID-TIPRLIST-U NOT = ALL '+'                                
057100             MOVE 'AAMMDD'       TO DAT-KDDATFORM                         
057200             MOVE MID-TIPRLIST-U TO DAT-I-TIDATUM                         
057300             CALL WDATKONV USING    DAT-KDDATFORM                         
057400                                    DAT-I-TIDATUM                         
057500                                    DAT-O-TIDATUM                         
057600                                    DAT-KDSVAR                            
057700             IF DAT-KDSVAR-FEL                                            
057800               MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRLIST-ATTR                
057900               MOVE 'GB'              TO MED-IDSKYLT                      
058000               MOVE ERR-WRONG-DATA    TO MED-IDMFSFEL                     
058100               CALL WMEDKONV USING MED-WMEDAREA                           
058200               MOVE MED-MFSFEL        TO MOD-TEMFSFEL                     
058300               MOVE JA                TO FLFEL-FAELT                      
058400             ELSE                                                         
058500               MOVE MFS-NUM-FAELT-RAETT TO                                
058600                          MOD-TIPRLIST-ATTR                               
058700             END-IF                                                       
058800           ELSE                                                           
058900             MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRLIST-ATTR                  
059000             MOVE 'GB'              TO MED-IDSKYLT                        
059100             MOVE ERR-WRONG-DATA    TO MED-IDMFSFEL                       
059200             CALL WMEDKONV USING MED-WMEDAREA                             
059300             MOVE MED-MFSFEL        TO MOD-TEMFSFEL                       
059400             MOVE JA      TO FLFEL-FAELT                                  
059500           END-IF                                                         
059600                                                                          
059700* **          HÄR KONTROLLERAS NYTT BEST.PRIS SÅ ATT ANTALET **           
059800* **          HELTALSSIFFROR OCH ANTALET DECIMALER INTE      **           
059900* **          ÖVERSKRIDER MAXANTAL TILLÅTNA                  **           
060000* **     OBS FÖR ATT KLARA 5 DEC ANVÄNDS WDECSTR I STÄLLET   **           
060100                                                                          
060200           IF MID-PRARTBEL-U NOT = ALL '+'                                
060300             MOVE MID-PRARTBEL-U TO STR-IDFRIDATA                         
060400             MOVE +8             TO STR-KVHELTAL                          
060500             MOVE +5             TO STR-KVDECIMAL                         
060600             MOVE NEJ            TO STR-KDSIGNAT                          
060700             MOVE NEJ            TO STR-KDLEFTJUST                        
060800             CALL WDECSTR  USING STR-WDECSTR                              
060900             IF STR-KDSVAR-FEL                                            
061000               MOVE MFS-NUM-FAELT-FEL TO                                  
061100                      MOD-PRARTBEL-ATTR                                   
061200               MOVE 'GB'              TO MED-IDSKYLT                      
061300               MOVE ERR-WRONG-DATA    TO MED-IDMFSFEL                     
061400               CALL WMEDKONV USING MED-WMEDAREA                           
061500               MOVE MED-MFSFEL        TO MOD-TEMFSFEL                     
061600               MOVE JA      TO FLFEL-FAELT                                
061700             ELSE                                                         
061800               MOVE STR-IDSTRDATA       TO W-IDSTRDATA-X                  
061900               IF W-IDSTRDATA-N <= 0                                      
062000                 MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTBEL-ATTR              
062100                 MOVE JA                TO FLFEL-FAELT                    
062200                 MOVE 'GB'              TO MED-IDSKYLT                    
062300                 MOVE ERR-WRONG-DATA    TO MED-IDMFSFEL                   
062400                 CALL WMEDKONV USING MED-WMEDAREA                         
062500                 MOVE MED-MFSFEL        TO MOD-TEMFSFEL                   
062600               ELSE                                                       
062700                 MOVE W-IDSTRDATA-N     TO W-PRARTBEL                     
062800                 MOVE MFS-NUM-FAELT-RAETT TO MOD-PRARTBEL-ATTR            
062900               END-IF                                                     
063000             END-IF                                                       
063100           ELSE                                                           
063200             MOVE MFS-NUM-FAELT-FEL TO                                    
063300                    MOD-PRARTBEL-ATTR                                     
063400             MOVE 'GB'              TO MED-IDSKYLT                        
063500             MOVE ERR-WRONG-DATA    TO MED-IDMFSFEL                       
063600             CALL WMEDKONV USING MED-WMEDAREA                             
063700             MOVE MED-MFSFEL        TO MOD-TEMFSFEL                       
063800             MOVE JA                TO FLFEL-FAELT                        
063900           END-IF                                                         
064000                                                                          
064100* **          HÄR KONTROLLERAS VALUTA                        **           
064200                                                                          
064300           IF MID-KDVALISO-U NOT = ALL '+'                                
064400             MOVE MFS-ALFA-FAELT-RAETT TO                                 
064500                    MOD-KDVALISO-ATTR                                     
064600           ELSE                                                           
064700             MOVE MFS-ALFA-FAELT-FEL   TO                                 
064800                    MOD-KDVALISO-ATTR                                     
064900             MOVE 'GB'                 TO MED-IDSKYLT                     
065000             MOVE ERR-WRONG-DATA       TO MED-IDMFSFEL                    
065100             CALL WMEDKONV USING MED-WMEDAREA                             
065200             MOVE MED-MFSFEL           TO MOD-TEMFSFEL                    
065300             MOVE JA                   TO FLFEL-FAELT                     
065400           END-IF                                                         
065500                                                                          
065600* **          DC KONTROLLERADE HÄR                           **           
065700                                                                          
065800           IF MID-IDDC-U     NOT = ALL '+'                                
065900             MOVE MFS-ALFA-FAELT-RAETT TO                                 
066000                    MOD-IDDC-ATTR                                         
066100           ELSE                                                           
066200             MOVE MFS-ALFA-FAELT-FEL   TO                                 
066300                    MOD-IDDC-ATTR                                         
066400             MOVE 'GB'                 TO MED-IDSKYLT                     
066500             MOVE ERR-WRONG-DATA       TO MED-IDMFSFEL                    
066600             CALL WMEDKONV USING MED-WMEDAREA                             
066700             MOVE MED-MFSFEL           TO MOD-TEMFSFEL                    
066800             MOVE JA                   TO FLFEL-FAELT                     
066900           END-IF                                                         
067000                                                                          
067100         END-IF                                                           
067200********   HÄR KONTOLLERAS ATT LEVNR FINNS PÅ WDF5 ****                   
067300                                                                          
067400         IF MID-IDLEVNR-IN NOT = ALL '+' AND                              
067500            MID-IDLEVNR-IN NOT = SPACE                                    
067600           PERFORM IMS-GU-WLLEVA01                                        
067700           IF SEGMENT-SAKNAS                                              
067800             MOVE 'GB'                 TO MED-IDSKYLT                     
067900             MOVE ERR-VENDOR-MISSING   TO MED-IDMFSFEL                    
068000             CALL WMEDKONV USING MED-WMEDAREA                             
068100             MOVE MED-MFSFEL           TO MOD-TEMFSFEL                    
068200             PERFORM MFS-RENSA-FAELT-UTDATA                               
068300             PERFORM MFS-RENSA-FAELT-INDATA                               
068400             MOVE JA                   TO FLFEL-FAELT                     
068500           END-IF                                                         
068600         END-IF                                                           
068700                                                                          
068800       ELSE                                                               
068900         MOVE 'GB'              TO MED-IDSKYLT                            
069000         MOVE ERR-PART-MISSING  TO MED-IDMFSFEL                           
069100         CALL WMEDKONV USING MED-WMEDAREA                                 
069200         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
069300         PERFORM MFS-RENSA-FAELT-UTDATA                                   
069400         PERFORM MFS-RENSA-FAELT-INDATA                                   
069500         MOVE JA      TO FLFEL-FAELT                                      
069600       END-IF                                                             
069700     END-IF                                                               
069800                                                                          
069900* **          HÄR KONTROLLERAS HUVUDLEVERANTÖR               **           
070000                                                                          
070100     IF FLFEL-FAELT = NEJ                                                 
070200       IF ART-IDLEVNR = W-IDLEVNR                                         
070300         MOVE 'GB'               TO MED-IDSKYLT                           
070400         MOVE ERR-VENDOR-MISSING TO MED-IDMFSFEL                          
070500         CALL WMEDKONV USING MED-WMEDAREA                                 
070600         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
070700         PERFORM MFS-RENSA-FAELT-UTDATA                                   
070800         PERFORM MFS-RENSA-FAELT-INDATA                                   
070900         MOVE JA                 TO FLFEL-FAELT                           
071000       END-IF                                                             
071100     END-IF                                                               
071200     .                                                                    
071300     EJECT                                                                
071400 CB-KONTROLLERA-MID-MOT-BAS SECTION.                                      
071500     SKIP3                                                                
071600     MOVE 'CB-KONTROLLERA-MID-MOT-BAS'  TO WS-SEKTION                     
071700                                                                          
071800     MOVE MID-IDDC-U       TO W-IDDC-B6                                   
071900     PERFORM IMS-GU-WDB601                                                
072000     IF SEGMENT-FINNS                                                     
072100        MOVE DCS-KDVALISO       TO CURR-KDVALISO-HUV                      
072200        IF W-IDDC-B6 = '61'                                               
072300                    OR '62'                                               
072400                    OR '6A'                                               
072500           MOVE 'SEK'           TO CURR-KDVALISO-HUV                      
072600        END-IF                                                            
072700        MOVE MID-KDVALISO-U     TO CURR-KDVALISO-ROW                      
072800        MOVE MID-TIPRLIST-U(1:4) TO CURR-TIAAMM                           
072900        MOVE 'M'                 TO CURR-KDVALTYP                         
073000        CALL W510CURR USING CURR-W510CURR 9305-PCB                        
073100        IF CURR-KDSVAR = ' '                                              
073200          CONTINUE                                                        
073300        ELSE                                                              
073400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDVALISO-ATTR                   
073500           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-ATTR                       
073600           MOVE 'GB'                 TO MED-IDSKYLT                       
073700           MOVE ERR-KDVALISO-MISSING TO MED-IDMFSFEL                      
073800           CALL WMEDKONV USING MED-WMEDAREA                               
073900           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
074000           MOVE JA                   TO FLFEL-FAELT                       
074100        END-IF                                                            
074200     END-IF                                                               
074300*                                                                         
074400* ** HÄR KONTROLLERAS ATT KURS FINNS UPPLAGD FÖR       **                 
074500* ** INMATAD VALUTA ELLER ATT KURS FINNS FÖR VALUTA    **                 
074600* ** SOM GÄLLER FÖR INKÖPSLAND.                        **                 
074700     MOVE MID-IDDC-U       TO W-IDDC-B6                                   
074800     PERFORM IMS-GU-WDB601                                                
074900     IF SEGMENT-FINNS                                                     
075000***     MOVE DCS-KDVALISO       TO CURR-KDVALISO-HUV                      
075100***     IF W-IDDC-B6 = '61'                                               
075200***                 OR '62'                                               
075300***                 OR '6A'                                               
075400        MOVE 'SEK'           TO CURR-KDVALISO-HUV                         
075500***     END-IF                                                            
075600        MOVE MID-KDVALISO-U     TO CURR-KDVALISO-ROW                      
075700        MOVE MID-TIPRLIST-U(1:4) TO CURR-TIAAMM                           
075800        MOVE 'M'                 TO CURR-KDVALTYP                         
075900        CALL W510CURR USING CURR-W510CURR 9305-PCB                        
076000        IF CURR-KDSVAR = ' '                                              
076100          MOVE CURR-PRKURS-NEW    TO W-PRKURS                             
076200          MOVE CURR-REVALUTA-TO   TO W-REVALUTA                           
076300          COMPUTE W-PRARTBES ROUNDED = W-PRARTBEL *                       
076400                                       W-PRKURS / W-REVALUTA              
076500        ELSE                                                              
076600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDVALISO-ATTR                   
076700           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-ATTR                       
076800           MOVE 'GB'                 TO MED-IDSKYLT                       
076900           MOVE ERR-KDVALISO-MISSING TO MED-IDMFSFEL                      
077000           CALL WMEDKONV USING MED-WMEDAREA                               
077100           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
077200           MOVE JA                   TO FLFEL-FAELT                       
077300        END-IF                                                            
077400     ELSE                                                                 
077500        IF SEGMENT-SAKNAS                                                 
077600           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-ATTR                       
077700           MOVE 'GB'                 TO MED-IDSKYLT                       
077800           MOVE ERR-WRONG-DC         TO MED-IDMFSFEL                      
077900           CALL WMEDKONV USING MED-WMEDAREA                               
078000           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
078100           MOVE JA                   TO FLFEL-FAELT                       
078200        END-IF                                                            
078300     END-IF                                                               
078400     PERFORM IMS-GU-WLLEVA01                                              
078500     IF SEGMENT-SAKNAS OR W-IDLEVNR = SPACES                              
078600        MOVE 'GB'                 TO MED-IDSKYLT                          
078700        MOVE ERR-VENDOR-MISSING   TO MED-IDMFSFEL                         
078800        CALL WMEDKONV USING MED-WMEDAREA                                  
078900        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
079000        MOVE JA                   TO FLFEL-FAELT                          
079100     END-IF                                                               
079200     .                                                                    
079300     EJECT                                                                
079400 CE-UPPDATERA SECTION.                                                    
079500     MOVE 'CE-UPPDATERA'   TO WS-SEKTION                                  
079600                                                                          
079700     IF (MID-TIPRLIST-U NOT = ALL '+')                                    
079800                                                                          
079900       PERFORM CEA-REDIGERA                                               
080000       PERFORM IMS-GU-WLARTC01                                            
080100       IF SEGMENT-FINNS                                                   
080200         IF (MID-TIPRLIST-U NOT = ALL '+')                                
080300           MOVE ART-KDPRODSL     TO TEST-KDPRODSL                         
080400           IF KDPRODSL-LOCAL                                              
080500             MOVE JA         TO LOKAL-PRODUKT                             
080600           END-IF                                                         
080700           PERFORM IMS-GNP-WLARTC21                                       
080800           IF SEGMENT-FINNS                                               
080900             MOVE JA         TO PRIS-FINNS                                
081000           ELSE                                                           
081100             MOVE NEJ        TO PRIS-FINNS                                
081200           END-IF                                                         
081300           PERFORM CEB-UPPDATERA-WLARTC21                                 
081400           IF FLFEL-FAELT = JA                                            
081500             CONTINUE                                                     
081600           ELSE                                                           
081700             IF LOKAL-PRODUKT = JA                                        
081800**** USA AND CHINA DOESN'T REPORT LOCAL PRODUCT ON 5119                   
081900               IF NDC-CN                                                  
082000               OR NDC-US                                                  
082100                  CONTINUE                                                
082200               ELSE                                                       
082300                  PERFORM IMS-GHU-WLARTS11                                
082400                  IF SEGMENT-FINNS                                        
082500                    MOVE IDLEVNR-WS     TO SLAG-IDLEVNR                   
082600                    PERFORM IMS-REPL-WLARTS11                             
082700                  END-IF                                                  
082800                  MOVE MID-IDDC-U       TO W-IDDC-B6                      
082900                  PERFORM IMS-GU-WDB601                                   
083000                   IF SEGMENT-FINNS                                       
083100                     MOVE DCS-IDLANDX2 TO W-IDLANDX2                      
083200                     PERFORM IMS-GHU-WDK712                               
083300                     IF SEGMENT-FINNS                                     
083400                      MOVE DAT-TIAA            TO TMP1-YY                 
083500                      MOVE DAGENS-DATUM(1:2)   TO TMP2-YY                 
083600                      PERFORM WY2000P9                                    
083700                      IF TMP1-YY < TMP2-YY                                
083800                        MOVE DAGENS-DATUM(1:2)  TO                        
083900                                                W-DATE-AAMM(1:2)          
084000                      ELSE                                                
084100                        MOVE DAT-TIAA           TO                        
084200                                                W-DATE-AAMM(1:2)          
084300                      END-IF                                              
084400                      MOVE 01                  TO W-DATE-AAMM(3:2)        
084500                      MOVE MID-KDVALISO-U     TO CURR-KDVALISO-ROW        
084600                      MOVE 'SEK'              TO CURR-KDVALISO-HUV        
084700                      MOVE W-DATE-AAMM         TO CURR-TIAAMM             
084800                      MOVE 'A'                 TO CURR-KDVALTYP           
084900                      CALL W510CURR USING CURR-W510CURR 9305-PCB          
085000                       IF CURR-KDSVAR = ' '                               
085100                                                                          
085200                         MOVE CURR-PRKURS-NEW    TO W-PRKURS              
085300                         MOVE CURR-REVALUTA-TO   TO W-REVALUTA            
085400                       END-IF                                             
085500                       COMPUTE LART-PRMATRL ROUNDED = W-PRARTBES *        
085600                                W-PRKURS / W-REVALUTA                     
085700                       PERFORM IMS-REPL-WDK712                            
085800                      END-IF                                              
085900                     END-IF                                               
086000                                                                          
086100               END-IF                                                     
086200               IF PRIS-FINNS = NEJ                                        
086300                 PERFORM S04-FOERSTA-PRISRAD                              
086400               ELSE                                                       
086500                 PERFORM S05-EJ-FOERSTA-PRISRAD                           
086600               END-IF                                                     
086700             END-IF                                                       
086800             IF MFS-UPDATE                                                
086900               PERFORM IMS-GU-WLARTC01                                    
087000               PERFORM IMS-GNP-WLARTC11                                   
087100               MOVE CLAG-IDANSK    TO MOD-IDANSK                          
087200               MOVE CLAG-IDINK     TO MOD-IDINK                           
087300               MOVE CLAG-PRINK     TO MOD-PRINK                           
087400               MOVE CLAG-PRARTSTD  TO MOD-PRARTSTD                        
087500               MOVE CLAG-PRARTSJK  TO MOD-PRARTSJK                        
087600               MOVE CLAG-KDERS     TO W-KDERS                             
087700               MOVE CLAG-KDPSLLOC  TO MOD-KDPSLLOC                        
087800               PERFORM S01-BEHANDLA-BESTPRIS-INFO                         
087900             END-IF                                                       
088000           END-IF                                                         
088100         END-IF                                                           
088200       END-IF                                                             
088300     END-IF                                                               
088400     .                                                                    
088500     EJECT                                                                
088600 CEA-REDIGERA  SECTION.                                                   
088700                                                                          
088800     MOVE 'CEA-REDIGERA'     TO WS-SEKTION                                
088900                                                                          
089000     IF MID-TIPRLIST-U NOT = ALL '+'                                      
089100       IF W-PRARTBEL = +0                                                 
089200         MOVE MID-PRARTBEL-U TO STR-IDFRIDATA                             
089300         MOVE +8             TO STR-KVHELTAL                              
089400         MOVE +5             TO STR-KVDECIMAL                             
089500         MOVE NEJ            TO STR-KDSIGNAT                              
089600         MOVE JA             TO STR-KDLEFTJUST                            
089700         CALL WDECSTR  USING STR-WDECSTR                                  
089800         IF STR-KDSVAR-OK                                                 
089900           MOVE STR-IDSTRDATA  TO W-IDSTRDATA-X                           
090000           MOVE W-IDSTRDATA-N  TO W-PRARTBEL                              
090100         END-IF                                                           
090200       END-IF                                                             
090300       IF W-PRKURS = +0                                                   
090400         MOVE 'AAMMDD' TO DAT-KDDATFORM                                   
090500         MOVE MID-TIPRLIST-U TO DAT-I-TIDATUM                             
090600         CALL WDATKONV USING    DAT-KDDATFORM                             
090700                                DAT-I-TIDATUM                             
090800                                DAT-O-TIDATUM                             
090900                                DAT-KDSVAR                                
091000         MOVE DAT-TIAA            TO TMP1-YY                              
091100         MOVE DAGENS-DATUM(1:2)   TO TMP2-YY                              
091200         PERFORM WY2000P9                                                 
091300         IF TMP1-YY < TMP2-YY                                             
091400           MOVE DAGENS-DATUM(1:2)  TO W-DATE-AAMM(1:2)                    
091500         ELSE                                                             
091600           MOVE DAT-TIAA           TO W-DATE-AAMM(1:2)                    
091700         END-IF                                                           
091800         MOVE 01                  TO W-DATE-AAMM(3:2)                     
091900         MOVE MID-KDVALISO-U     TO CURR-KDVALISO-ROW                     
092000         MOVE 'SEK'              TO CURR-KDVALISO-HUV                     
092100         MOVE W-DATE-AAMM         TO CURR-TIAAMM                          
092200         MOVE 'A'                 TO CURR-KDVALTYP                        
092300         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
092400         IF CURR-KDSVAR = ' '                                             
092500           MOVE CURR-PRKURS-NEW    TO W-PRKURS                            
092600           MOVE CURR-REVALUTA-TO   TO W-REVALUTA                          
092700         END-IF                                                           
092800       END-IF                                                             
092900                                                                          
093000       COMPUTE W-PRARTBES ROUNDED = W-PRARTBEL *                          
093100                                    W-PRKURS / W-REVALUTA                 
093200     END-IF                                                               
093300       .                                                                  
093400     EJECT                                                                
093500 CEB-UPPDATERA-WLARTC21 SECTION.                                          
093600                                                                          
093700     MOVE 'CEB-UPPDATERA-WLARTC21'  TO WS-SEKTION                         
093800                                                                          
093900* ** BEST.PRISER -- WLARTC21 -- WDK621                          **        
094000                                                                          
094100     PERFORM IMS-GU-WLARTC11                                              
094200     MOVE 1 TO INDX                                                       
094300     MOVE NEJ TO FLSLUTA-LAS                                              
094400     MOVE MID-TIPRLIST-U      TO W-DAPRLIST-AKT                           
094500     IF W-AAR < 50                                                        
094600       MOVE 20                TO W-SEKEL                                  
094700     ELSE                                                                 
094800       MOVE 19                TO W-SEKEL                                  
094900     END-IF                                                               
095000     SUBTRACT W-DAPRLIST-AKT FROM W-DAPRLIST-MAX                          
095100                                 GIVING W-DAPRLIST-21                     
095200     MOVE W-IDLEVNR           TO W-IDLEVNR-21                             
095300     PERFORM IMS-GHU-WLARTC21                                             
095400     IF SEGMENT-FINNS                                                     
095500* **       UPPDATERING AV BEST.PRIS                          **           
095600                                                                          
095700       MOVE 'N'               TO PRL-FLHUVLEV                             
095800       MOVE MID-KDPRURSP-U    TO PRL-KDPRURSP                             
095900       MOVE W-PRARTBEL        TO PRL-PRARTBEL-PR                          
096000       MOVE W-PRARTBES        TO PRL-PRARTBES-PR                          
096100       MOVE 1                 TO PRL-KDSTATUS-PR                          
096200       MOVE ZERO              TO PRL-SUINLEV-PR                           
096300       MOVE MID-KDVALISO-U    TO PRL-KDVALISO                             
096400       MOVE DAGENS-DATUM      TO PRL-TIREGDAT                             
096500       MOVE MSGI-IDUSER       TO PRL-IDUSER                               
096600       PERFORM IMS-REPL-WLARTC21                                          
096700     ELSE                                                                 
096800* **       NYUPPLÄGG AV BEST. PRIS                              **        
096900                                                                          
097000       PERFORM S03-INIT-WDK621                                            
097100       MOVE 1                 TO INDX                                     
097200       MOVE W-IDLEVNR         TO PRL-IDLEVNR                              
097300       MOVE MID-TIPRLIST-U    TO W-DAPRLIST-AKT                           
097400       IF W-AAR < 50                                                      
097500         MOVE 20                TO W-SEKEL                                
097600       ELSE                                                               
097700         MOVE 19                TO W-SEKEL                                
097800       END-IF                                                             
097900       SUBTRACT W-DAPRLIST-AKT FROM W-DAPRLIST-MAX                        
098000                                 GIVING W-DAPRLIST-21                     
098100       MOVE W-DAPRLIST-21     TO PRL-DAPRLIST-9KOMPL                      
098200       MOVE 'N'               TO PRL-FLHUVLEV                             
098300       MOVE W-PRARTBES        TO PRL-PRARTBES-PR                          
098400       MOVE W-PRARTBEL        TO PRL-PRARTBEL-PR                          
098500       MOVE 1                 TO PRL-KDSTATUS-PR                          
098600       MOVE ZERO              TO PRL-SUINLEV-PR                           
098700       MOVE MID-KDPRURSP-U    TO PRL-KDPRURSP                             
098800       MOVE MID-KDVALISO-U    TO PRL-KDVALISO                             
098900       MOVE SPACE             TO PRL-KDFPKPRI                             
099000       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
099100               MOD-KDPRURSP-PR-ATTR (INDX)                                
099200               MOD-TIPRLIST-PR-ATTR (INDX)                                
099300               MOD-PRARTBEL-PR-ATTR (INDX)                                
099400               MOD-KDVALISO-PR-ATTR (INDX)                                
099500       MOVE DAGENS-DATUM      TO PRL-TIREGDAT                             
099600       MOVE MSGI-IDUSER       TO PRL-IDUSER                               
099700       PERFORM IMS-ISRT-WLARTC21                                          
099800       IF SEGMENT-FINNS-REDAN                                             
099900         MOVE 'GB'           TO MED-IDSKYLT                               
100000         MOVE 'DUPLICATE DATES'  TO MED-IDMFSFEL                          
100100         CALL WMEDKONV USING MED-WMEDAREA                                 
100200         MOVE MED-MFSFEL     TO MOD-TEMFSFEL                              
100300         MOVE JA             TO FLFEL-FAELT                               
100400       END-IF                                                             
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800 D-SOEKNING  SECTION.                                                     
100900                                                                          
101000     PERFORM IMS-GU-WLARTC01                                              
101100     IF SEGMENT-FINNS                                                     
101200       IF ART-KDERS-UTG = 0                                               
101300         MOVE ART-IDLEVNR  TO MOD-IDLEVNR                                 
101400         IF ART-IDLEVNR = W-IDLEVNR                                       
101500           MOVE 'GB'               TO MED-IDSKYLT                         
101600           MOVE ERR-VENDOR-MISSING TO MED-IDMFSFEL                        
101700           CALL WMEDKONV USING MED-WMEDAREA                               
101800           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
101900           PERFORM MFS-RENSA-FAELT-UTDATA                                 
102000           PERFORM MFS-RENSA-FAELT-INDATA                                 
102100         ELSE                                                             
102200           PERFORM IMS-GU-WLLEVA01                                        
102300           IF SEGMENT-FINNS                                               
102400             PERFORM DA-BEHANDLA-OVRIGA-SEGMENT                           
102500             PERFORM MFS-RENSA-FAELT-INDATA                               
102600             PERFORM MFS-OEPPNA-FAELT-INDATA                              
102700           ELSE                                                           
102800             MOVE 'GB'               TO MED-IDSKYLT                       
102900             MOVE ERR-VENDOR-MISSING TO MED-IDMFSFEL                      
103000             CALL WMEDKONV USING MED-WMEDAREA                             
103100             MOVE MED-MFSFEL         TO MOD-TEMFSFEL                      
103200             PERFORM MFS-RENSA-FAELT-UTDATA                               
103300             PERFORM MFS-RENSA-FAELT-INDATA                               
103400           END-IF                                                         
103500           PERFORM IMS-GU-WLLEVA01                                        
103600           IF SEGMENT-FINNS AND W-IDLEVNR = SPACES                        
103700             MOVE 'GB'               TO MED-IDSKYLT                       
103800             MOVE ERR-VENDOR-MISSING TO MED-IDMFSFEL                      
103900             CALL WMEDKONV USING MED-WMEDAREA                             
104000             MOVE MED-MFSFEL         TO MOD-TEMFSFEL                      
104100*            PERFORM MFS-RENSA-FAELT-UTDATA                               
104200*            PERFORM MFS-RENSA-FAELT-INDATA                               
104300           END-IF                                                         
104400         END-IF                                                           
104500       ELSE                                                               
104600         MOVE 'GB'                 TO MED-IDSKYLT                         
104700         MOVE ERR-PART-SUPERSEDED  TO MED-IDMFSFEL                        
104800         CALL WMEDKONV USING MED-WMEDAREA                                 
104900         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
105000         PERFORM MFS-RENSA-FAELT-UTDATA                                   
105100         PERFORM MFS-RENSA-FAELT-INDATA                                   
105200       END-IF                                                             
105300     ELSE                                                                 
105400       MOVE 'GB'              TO MED-IDSKYLT                              
105500       MOVE ERR-PART-MISSING  TO MED-IDMFSFEL                             
105600       CALL WMEDKONV USING MED-WMEDAREA                                   
105700       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
105800       PERFORM MFS-RENSA-FAELT-UTDATA                                     
105900       PERFORM MFS-RENSA-FAELT-INDATA                                     
106000     END-IF                                                               
106100     .                                                                    
106200     EJECT                                                                
106300 DA-BEHANDLA-OVRIGA-SEGMENT SECTION.                                      
106400                                                                          
106500     MOVE 'DA-BEHANDLA-OVRIGA-SEGMENT' TO WS-SEKTION                      
106600                                                                          
106700     PERFORM IMS-GU-WLBENA11                                              
106800                                                                          
106900     IF SEGMENT-FINNS                                                     
107000       MOVE BEN-TEXT-BEART  TO MOD-BEART                                  
107100     ELSE                                                                 
107200       MOVE MFS-RENSA-FAELT TO MOD-BEART                                  
107300     END-IF                                                               
107400                                                                          
107500     PERFORM IMS-GNP-WLARTC11                                             
107600                                                                          
107700     IF SEGMENT-FINNS                                                     
107800       MOVE CLAG-IDANSK            TO MOD-IDANSK                          
107900       MOVE CLAG-IDINK             TO MOD-IDINK                           
108000       MOVE CLAG-PRINK             TO MOD-PRINK                           
108100       MOVE CLAG-PRARTSTD          TO MOD-PRARTSTD                        
108200       MOVE CLAG-PRARTSJK          TO MOD-PRARTSJK                        
108300       MOVE CLAG-KDERS             TO W-KDERS                             
108400       MOVE CLAG-KDPSLLOC          TO MOD-KDPSLLOC                        
108500                                                                          
108600       IF W-KDERS > 9                                                     
108700         MOVE 'GB'              TO MED-IDSKYLT                            
108800         MOVE ERR-PART-EXPIRE   TO MED-IDMFSFEL                           
108900         CALL WMEDKONV USING MED-WMEDAREA                                 
109000         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
109100       END-IF                                                             
109200     END-IF                                                               
109300                                                                          
109400     PERFORM S01-BEHANDLA-BESTPRIS-INFO                                   
109500                                                                          
109600     .                                                                    
109700     EJECT                                                                
109800 S01-BEHANDLA-BESTPRIS-INFO SECTION.                                      
109900                                                                          
110000     MOVE 'S-BEHANDLA-BESTPRIS-INFO' TO WS-SEKTION                        
110100                                                                          
110200     MOVE 1 TO INDX                                                       
110300     PERFORM IMS-GNP-WLARTC21-OKVAL                                       
110400     PERFORM UNTIL INDX > 5                                               
110500       IF SEGMENT-FINNS                                                   
110600         IF PRL-IDLEVNR = WS-IDLEVNR                                      
110700           SUBTRACT PRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX               
110800                                   GIVING W-DAPRLIST-21                   
110900           MOVE W-DAPRLIST-21   TO MOD-TIPRLIST-PR (INDX)                 
111000           MOVE PRL-PRARTBEL-PR TO MOD-PRARTBEL-PR (INDX)                 
111100           MOVE PRL-KDPRURSP    TO MOD-KDPRURSP-PR (INDX)                 
111200           MOVE PRL-KDVALISO    TO MOD-KDVALISO-PR (INDX)                 
111300           ADD +1               TO W-ANT-EJ-HUVLEV-RADER                  
111400           IF MFS-UPDATE                                                  
111500             IF MID-TIPRLIST-U = W-DAPRLIST-21                            
111600               MOVE MFS-ADD-LYS-UPP-FAELT TO                              
111700                    MOD-KDPRURSP-PR-ATTR (INDX)                           
111800                    MOD-TIPRLIST-PR-ATTR (INDX)                           
111900                    MOD-PRARTBEL-PR-ATTR (INDX)                           
112000                    MOD-KDVALISO-PR-ATTR (INDX)                           
112100             END-IF                                                       
112200           END-IF                                                         
112300           ADD 1 TO INDX                                                  
112400         END-IF                                                           
112500                                                                          
112600* PRARTBES BORTTAGET FRÅN K611                                            
112700* DÄRFÖR HÄMTAS PRARTBES-PR FRÅN K621 TILL MOD-PRARTBES                   
112800         MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD               
112900         COMPUTE W-DAPRLIST-21 = 99999999 - DAGENS-AAAAMMDD               
113000                                                                          
113100         IF PRL-DAPRLIST-9KOMPL NOT < W-DAPRLIST-21                       
113200           IF MOD-PRARTBES (9:2) NOT NUMERIC OR FIRST-INLEV = NEJ         
113300             IF MOD-PRARTBES (9:2) NOT NUMERIC                            
113400               MOVE PRL-PRARTBES-PR TO MOD-PRARTBES                       
113500             END-IF                                                       
113600             IF PRL-SUINLEV-PR > ZERO                                     
113700               MOVE JA  TO FIRST-INLEV                                    
113800               MOVE PRL-PRARTBES-PR TO MOD-PRARTBES                       
113900             ELSE                                                         
114000               MOVE NEJ TO FIRST-INLEV                                    
114100             END-IF                                                       
114200           END-IF                                                         
114300         ELSE                                                             
114400           MOVE CLAG-PRARTSTD TO MOD-PRARTBES                             
114500         END-IF                                                           
114600* SLUT PRARTBES BORTTAGET FRÅN K611                                       
114700                                                                          
114800         IF INDX < 6                                                      
114900           PERFORM IMS-GNP-WLARTC21-OKVAL                                 
115000         END-IF                                                           
115100       ELSE                                                               
115200         PERFORM UNTIL INDX > 5                                           
115300           MOVE MFS-RENSA-FAELT TO MOD-KDPRURSP-PR (INDX)                 
115400           MOVE MFS-RENSA-FAELT TO MOD-TIPRLIST-PR (INDX)                 
115500           MOVE MFS-RENSA-FAELT TO MOD-PRARTBEL-PR (INDX)                 
115600           MOVE MFS-RENSA-FAELT TO MOD-KDVALISO-PR (INDX)                 
115700           ADD 1 TO INDX                                                  
115800         END-PERFORM                                                      
115900       END-IF                                                             
116000     END-PERFORM                                                          
116100                                                                          
116200*** HÄR SKER RENSNING AV RAD 6 (WDK621-SEGMENT),                          
116300*** OM MAN LAGT UPP NYTT 21-SEGMENT OCH DET FÖRUT FANNS                   
116400*** 5 SEGMENT PÅ SAMMA LEVERANTÖR MED HUVUDLEVERANTÖR = NEJ               
116500                                                                          
116600     IF W-ANT-EJ-HUVLEV-RADER = +5                                        
116700       IF MFS-UPDATE OR MFS-UPD-X                                         
116800         PERFORM UNTIL NOT SEGMENT-FINNS                                  
116900           PERFORM IMS-GHNP-WLARTC21-OKVAL                                
117000           IF SEGMENT-FINNS                                               
117100             IF PRL-IDLEVNR = WS-IDLEVNR                                  
117200               IF PRL-FLHUVLEV = NEJ                                      
117300                   PERFORM IMS-DLET-WLARTC21                              
117400               END-IF                                                     
117500             END-IF                                                       
117600           END-IF                                                         
117700         END-PERFORM                                                      
117800       END-IF                                                             
117900     END-IF                                                               
118000     .                                                                    
118100     EJECT                                                                
118200 S03-INIT-WDK621   SECTION.                                               
118300                                                                          
118400     MOVE 'S03-INIT-WDK621  '  TO WS-SEKTION                              
118500                                                                          
118600     MOVE ZERO                 TO PRL-DAPRLIST-9KOMPL                     
118700     MOVE ZERO                 TO PRL-PRARTBES-PR                         
118800     MOVE ZERO                 TO PRL-PRARTBEL-PR                         
118900     MOVE ZERO                 TO PRL-KDSTATUS-PR                         
119000     MOVE ZERO                 TO PRL-SUINLEV-PR                          
119100     MOVE SPACE                TO PRL-KDPRURSP                            
119200     MOVE SPACE                TO PRL-KDVALISO                            
119300     MOVE SPACE                TO PRL-FLHUVLEV                            
119400     MOVE SPACE                TO PRL-KDFPKPRI                            
119500     MOVE ZERO                 TO PRL-TIREGDAT                            
119600     MOVE SPACES               TO PRL-IDUSER                              
119700                                                                          
119800     .                                                                    
119900     EJECT                                                                
120000 S04-FOERSTA-PRISRAD  SECTION.                                            
120100                                                                          
120200     MOVE 'S04-FOERSTA-PRISRAD'  TO WS-SEKTION                            
120300                                                                          
120400     PERFORM IMS-GHU-WLARTC11                                             
120500     IF CLAG-PRARTSTD NOT = ZERO                                          
120600       PERFORM S06-SALDO                                                  
120700       PERFORM S07-KONTROLLERA-SALDO                                      
120800                                                                          
120900       COMPUTE W-PRARTSJK = W-PRARTBES + CLAG-PRDIRLON                    
121000                          + CLAG-PRDMTRL + CLAG-PROVRPAL                  
121100       MOVE W-PRARTSJK           TO CLAG-PRARTSJK                         
121200                                                                          
121300       IF WS-LS-CDC = ZERO  AND  WS-LS-SDC = ZERO                         
121400         PERFORM IMS-GU-WLLEVA01                                          
121500         IF SEGMENT-FINNS                                                 
121600           MOVE 'SE' TO W-IDLAND                                          
121700           PERFORM IMS-GNP-WLLEVA11                                       
121800           IF SEGMENT-FINNS                                               
121900             MOVE LEV-TULL-TITULF  TO TMP1-YYMMDD                         
122000             MOVE DAGENS-DATUM   TO TMP2-YYMMDD                           
122100             PERFORM WY2000P1                                             
122200             IF TMP1-YYMMDD < TMP2-YYMMDD                                 
122300               MOVE LEV-TULL-RETULF-1  TO W-RETULF                        
122400             ELSE                                                         
122500               MOVE LEV-TULL-RETULF-2  TO W-RETULF                        
122600             END-IF                                                       
122700           ELSE                                                           
122800             MOVE 1                TO W-RETULF                            
122900           END-IF                                                         
123000         END-IF                                                           
123100         IF W-RETULF = ZERO                                               
123200           MOVE 1                  TO W-RETULF                            
123300         END-IF                                                           
123400         COMPUTE W-PRINK = (W-PRARTBEL * W-RETULF * W-PRKURS) /           
123500                            W-REVALUTA                                    
123600         MOVE W-PRINK            TO CLAG-PRINK                            
123700         IF ART-KDSORT = 'SW'                                             
123800           MOVE ZERO             TO CLAG-PRHEMTAG                         
123900         ELSE                                                             
124000           COMPUTE CLAG-PRHEMTAG ROUNDED =                                
124100             CLAG-PRINK * (W-RETULF - 1) / W-RETULF                       
124200           COMPUTE W-PRARTSTD = W-PRINK + CLAG-PRDIRLON +                 
124300                                CLAG-PRDMTRL + CLAG-PROVRPAL              
124400           MOVE W-PRARTSTD       TO CLAG-PRARTSTD                         
124500         END-IF                                                           
124600       END-IF                                                             
124700                                                                          
124800       PERFORM IMS-REPL-WLARTC11                                          
124900     END-IF                                                               
125000                                                                          
125100     .                                                                    
125200     EJECT                                                                
125300 S05-EJ-FOERSTA-PRISRAD  SECTION.                                         
125400                                                                          
125500     MOVE 'S05-EJ-FOERSTA-PRISRAD'  TO WS-SEKTION                         
125600                                                                          
125700     PERFORM IMS-GHU-WLARTC11                                             
125800                                                                          
125900     IF CLAG-PRARTSTD NOT = ZERO                                          
126000       COMPUTE W-PRARTSJK = W-PRARTBES + CLAG-PRDIRLON +                  
126100                            CLAG-PRDMTRL + CLAG-PROVRPAL                  
126200       MOVE W-PRARTSJK           TO CLAG-PRARTSJK                         
126300                                                                          
126400       PERFORM IMS-REPL-WLARTC11                                          
126500     END-IF                                                               
126600                                                                          
126700     .                                                                    
126800     EJECT                                                                
126900 S06-SALDO  SECTION.                                                      
127000                                                                          
127100     MOVE 'S06-SALDO'          TO WS-SEKTION                              
127200                                                                          
127300     ADD CLAG-KVAKS-CDC        TO WS-LS-CDC                               
127400     ADD CLAG-KVAKS-PAV        TO WS-LS-CDC                               
127500     ADD CLAG-KVAKS-T          TO WS-LS-CDC                               
127600     ADD CLAG-KVEFRS           TO WS-LS-CDC                               
127700     ADD CLAG-KVLS             TO WS-LS-CDC                               
127800     ADD CLAG-KVRESS           TO WS-LS-CDC                               
127900                                                                          
128000     IF CLAG-RETULF = +0                                                  
128100       MOVE 1                  TO W-RETULF                                
128200     ELSE                                                                 
128300       MOVE CLAG-RETULF        TO W-RETULF                                
128400     END-IF                                                               
128500                                                                          
128600     .                                                                    
128700     EJECT                                                                
128800 S07-KONTROLLERA-SALDO  SECTION.                                          
128900                                                                          
129000     MOVE 'S07-KONTROLLERA-SALDO'  TO WS-SEKTION                          
129100                                                                          
129200     PERFORM IMS-GU-WLARTS01                                              
129300     IF SEGMENT-FINNS                                                     
129400       PERFORM IMS-GNP-WLARTS11                                           
129500       PERFORM UNTIL SEGMENT-SAKNAS                                       
129600         MOVE SLAG-IDDC        TO W-IDDC-B6                               
129700         PERFORM IMS-GU-WDB601                                            
129800                                                                          
129900         IF DCS-SDC OR DCS-NDC-PF OR DCS-NDC-OTHERS                       
130000           IF DCS-INDIA                                                   
130100             CONTINUE                                                     
130200           ELSE                                                           
130300             ADD SLAG-KVAKS-PAV TO WS-LS-SDC                              
130400             ADD SLAG-KVAKS-SDC TO WS-LS-SDC                              
130500             ADD SLAG-KVEFRS   TO WS-LS-SDC                               
130600             ADD SLAG-KVLS     TO WS-LS-SDC                               
130700           END-IF                                                         
130800         END-IF                                                           
130900         PERFORM IMS-GNP-WLARTS11                                         
131000       END-PERFORM                                                        
131100     END-IF                                                               
131200     .                                                                    
131300     EJECT                                                                
131400 MFS-FORMATETS-ATTR-INDATA SECTION.                                       
131500     SKIP3                                                                
131600     MOVE MFS-FORMATETS-ATTR TO    MOD-KDPRURSP-ATTR                      
131700                                   MOD-TIPRLIST-ATTR                      
131800                                   MOD-PRARTBEL-ATTR                      
131900                                   MOD-KDVALISO-ATTR                      
132000                                   MOD-IDDC-ATTR                          
132100     .                                                                    
132200     EJECT                                                                
132300 MFS-OEPPNA-FAELT-INDATA SECTION.                                         
132400                                                                          
132500     MOVE MFS-OPEN-NUM-NOMOD    TO MOD-TIPRLIST-ATTR                      
132600     MOVE MFS-OPEN-ALPHA-NOMOD  TO MOD-PRARTBEL-ATTR                      
132700                                   MOD-KDVALISO-ATTR                      
132800                                   MOD-IDDC-ATTR                          
132900     .                                                                    
133000     EJECT                                                                
133100 MFS-ROER-EJ-FAELT-UTDATA SECTION.                                        
133200     SKIP3                                                                
133300     MOVE MFS-ROER-EJ-FAELT TO  MOD-BEART                                 
133400                                MOD-PRINK                                 
133500                                MOD-PRARTSTD                              
133600                                MOD-PRARTBES                              
133700                                MOD-PRARTSJK                              
133800                                MOD-KDPSLLOC                              
133900                                MOD-IDANSK                                
134000                                MOD-IDINK                                 
134100                                MOD-IDLEVNR                               
134200                                                                          
134300     MOVE 1                 TO INDX                                       
134400     PERFORM UNTIL INDX > 5                                               
134500       MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRURSP-PR (INDX)                   
134600                                 MOD-TIPRLIST-PR (INDX)                   
134700                                 MOD-PRARTBEL-PR (INDX)                   
134800                                 MOD-KDVALISO-PR (INDX)                   
134900       ADD 1 TO INDX                                                      
135000     END-PERFORM                                                          
135100                                                                          
135200     .                                                                    
135300     EJECT                                                                
135400 MFS-ROER-EJ-FAELT-INDATA SECTION.                                        
135500     SKIP3                                                                
135600                                                                          
135700     MOVE MFS-ROER-EJ-FAELT TO  MOD-KDPRURSP                              
135800                                MOD-TIPRLIST                              
135900                                MOD-PRARTBEL                              
136000                                MOD-KDVALISO                              
136100                                MOD-IDDC                                  
136200     .                                                                    
136300     EJECT                                                                
136400 MFS-RENSA-FAELT-UTDATA SECTION.                                          
136500     SKIP3                                                                
136600                                                                          
136700     MOVE MFS-RENSA-FAELT  TO   MOD-BEART                                 
136800                                MOD-PRINK                                 
136900                                MOD-PRARTSTD                              
137000                                MOD-PRARTBES                              
137100                                MOD-PRARTSJK                              
137200                                MOD-KDPSLLOC                              
137300                                MOD-IDANSK                                
137400                                MOD-IDINK                                 
137500                                MOD-IDLEVNR                               
137600                                                                          
137700     MOVE 1                 TO INDX                                       
137800     PERFORM UNTIL INDX > 5                                               
137900       MOVE MFS-RENSA-FAELT   TO MOD-KDPRURSP-PR (INDX)                   
138000                                 MOD-TIPRLIST-PR (INDX)                   
138100                                 MOD-PRARTBEL-PR (INDX)                   
138200                                 MOD-KDVALISO-PR (INDX)                   
138300       ADD 1 TO INDX                                                      
138400     END-PERFORM                                                          
138500                                                                          
138600     .                                                                    
138700     EJECT                                                                
138800 MFS-RENSA-FAELT-INDATA SECTION.                                          
138900     SKIP3                                                                
139000                                                                          
139100     MOVE MFS-RENSA-FAELT  TO   MOD-KDPRURSP                              
139200                                MOD-TIPRLIST                              
139300                                MOD-PRARTBEL                              
139400                                MOD-KDVALISO                              
139500                                MOD-IDDC                                  
139600     .                                                                    
139700     EJECT                                                                
139800 IMS-GET-MSG SECTION.                                                     
139900                                                                          
140000     MOVE '  QC' TO GODK-STATUSKODER                                      
140100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
140200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
140300     PERFORM IMS-STATUSKONTROLL                                           
140400     .                                                                    
140500     SKIP3                                                                
140600 IMS-GN-MSG-KOM SECTION.                                                  
140700                                                                          
140800     MOVE '  QD' TO GODK-STATUSKODER                                      
140900     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
141000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
141100     PERFORM IMS-STATUSKONTROLL                                           
141200     .                                                                    
141300     EJECT                                                                
141400 IMS-INSERT-MSG SECTION.                                                  
141500                                                                          
141600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
141700       MOVE '0' TO MFS-KDHUVOMR                                           
141800     END-IF                                                               
141900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
142000     MOVE SPACE TO GODK-STATUSKODER                                       
142100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
142200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
142300     PERFORM IMS-STATUSKONTROLL                                           
142400     .                                                                    
142500     SKIP3                                                                
142600 IMS-INSERT-MSG-KOM SECTION.                                              
142700                                                                          
142800     MOVE SPACE TO GODK-STATUSKODER                                       
142900     CALL CBLTDLI USING ISRT ALT-PCB KOM-IO-AREA                          
143000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
143100     PERFORM IMS-STATUSKONTROLL                                           
143200     .                                                                    
143300     EJECT                                                                
143400 IMS-GU-WLARTC01 SECTION.                                                 
143500                                                                          
143600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
143700     DELIMITED  BY SIZE INTO SSA1                                         
143800     MOVE '  GE' TO GODK-STATUSKODER                                      
143900     CALL CBLTDLI USING GU ARTC-PCB WLARTC01 SSA1                         
144000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
144100     PERFORM IMS-STATUSKONTROLL                                           
144200     .                                                                    
144300     SKIP3                                                                
144400 IMS-GNP-WLARTC11 SECTION.                                                
144500                                                                          
144600     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
144700     MOVE '    ' TO GODK-STATUSKODER                                      
144800     CALL CBLTDLI USING GNP ARTC-PCB WLARTC11 SSA1                        
144900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
145000     PERFORM IMS-STATUSKONTROLL                                           
145100     .                                                                    
145200     SKIP3                                                                
145300 IMS-GHU-WLARTC11 SECTION.                                                
145400                                                                          
145500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
145600     DELIMITED  BY SIZE INTO SSA1                                         
145700     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
145800     MOVE '    ' TO GODK-STATUSKODER                                      
145900     CALL CBLTDLI USING GHU ARTC-PCB WLARTC11 SSA1 SSA2                   
146000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
146100     PERFORM IMS-STATUSKONTROLL                                           
146200     .                                                                    
146300     SKIP3                                                                
146400 IMS-GU-WLARTC11 SECTION.                                                 
146500                                                                          
146600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
146700     DELIMITED  BY SIZE INTO SSA1                                         
146800     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
146900     MOVE '    ' TO GODK-STATUSKODER                                      
147000     CALL CBLTDLI USING GU ARTC-PCB WLARTC11 SSA1 SSA2                    
147100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
147200     PERFORM IMS-STATUSKONTROLL                                           
147300     .                                                                    
147400     SKIP3                                                                
147500 IMS-GNP-WLARTC21 SECTION.                                                
147600                                                                          
147700     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
147800     MOVE 'WLARTC21 ' TO SSA2                                             
147900     MOVE '  GE' TO GODK-STATUSKODER                                      
148000     CALL CBLTDLI USING GNP ARTC-PCB WLARTC21 SSA1 SSA2                   
148100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
148200     PERFORM IMS-STATUSKONTROLL                                           
148300     .                                                                    
148400     SKIP3                                                                
148500 IMS-GNP-WLARTC21-OKVAL SECTION.                                          
148600                                                                          
148700     MOVE 'WLARTC21 ' TO SSA1                                             
148800     MOVE '  GE' TO GODK-STATUSKODER                                      
148900     CALL CBLTDLI USING GNP ARTC-PCB WLARTC21 SSA1                        
149000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
149100     PERFORM IMS-STATUSKONTROLL                                           
149200     .                                                                    
149300     SKIP3                                                                
149400 IMS-GHNP-WLARTC21-OKVAL SECTION.                                         
149500                                                                          
149600     MOVE 'WLARTC21 ' TO SSA1                                             
149700     MOVE '  GE' TO GODK-STATUSKODER                                      
149800     CALL CBLTDLI USING GHNP ARTC-PCB WLARTC21 SSA1                       
149900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
150000     PERFORM IMS-STATUSKONTROLL                                           
150100     .                                                                    
150200     EJECT                                                                
150300 IMS-GHU-WLARTC21 SECTION.                                                
150400                                                                          
150500     STRING 'WLARTC21(WDK621KY =' W-WDK621KY-X ')'                        
150600     DELIMITED BY SIZE INTO SSA1                                          
150700     MOVE '  GE' TO GODK-STATUSKODER                                      
150800     CALL CBLTDLI USING GHU ARTC-PCB WLARTC21 SSA1                        
150900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
151000     PERFORM IMS-STATUSKONTROLL                                           
151100     .                                                                    
151200     EJECT                                                                
151300 IMS-ISRT-WLARTC21 SECTION.                                               
151400                                                                          
151500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
151600     DELIMITED  BY SIZE INTO SSA1                                         
151700     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
151800     MOVE 'WLARTC21 ' TO SSA3                                             
151900     MOVE '  II' TO GODK-STATUSKODER                                      
152000     CALL CBLTDLI USING ISRT ARTC-PCB WLARTC21 SSA1 SSA2 SSA3             
152100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
152200     PERFORM IMS-STATUSKONTROLL                                           
152300     .                                                                    
152400     SKIP3                                                                
152500 IMS-REPL-WLARTC11 SECTION.                                               
152600                                                                          
152700     MOVE '    ' TO GODK-STATUSKODER                                      
152800     CALL CBLTDLI USING REPL ARTC-PCB WLARTC11                            
152900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
153000     PERFORM IMS-STATUSKONTROLL                                           
153100     .                                                                    
153200     EJECT                                                                
153300 IMS-REPL-WLARTC21 SECTION.                                               
153400                                                                          
153500     MOVE '    ' TO GODK-STATUSKODER                                      
153600     CALL CBLTDLI USING REPL ARTC-PCB WLARTC21                            
153700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
153800     PERFORM IMS-STATUSKONTROLL                                           
153900     .                                                                    
154000     EJECT                                                                
154100 IMS-DLET-WLARTC21  SECTION.                                              
154200                                                                          
154300     MOVE '  GE' TO GODK-STATUSKODER                                      
154400     CALL CBLTDLI USING DLET ARTC-PCB WLARTC21                            
154500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
154600     PERFORM IMS-STATUSKONTROLL                                           
154700     .                                                                    
154800     SKIP3                                                                
154900 IMS-GU-WLARTS01 SECTION.                                                 
155000                                                                          
155100     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
155200     DELIMITED  BY SIZE INTO SSA1                                         
155300     MOVE '  GE' TO GODK-STATUSKODER                                      
155400     CALL CBLTDLI USING GU ARTS-PCB WLARTS01 SSA1                         
155500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
155600     PERFORM IMS-STATUSKONTROLL                                           
155700     .                                                                    
155800     SKIP3                                                                
155900 IMS-GNP-WLARTS11 SECTION.                                                
156000                                                                          
156100     MOVE 'WLARTS11' TO SSA1                                              
156200     MOVE '  GE' TO GODK-STATUSKODER                                      
156300     CALL CBLTDLI USING GNP ARTS-PCB WLARTS11 SSA1                        
156400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
156500     PERFORM IMS-STATUSKONTROLL                                           
156600     .                                                                    
156700     EJECT                                                                
156800 IMS-GHU-WLARTS11 SECTION.                                                
156900                                                                          
157000     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
157100          DELIMITED BY SIZE INTO SSA1                                     
157200     STRING 'WLARTS11(IDDC     =' W-IDDC-B6-X ')'                         
157300          DELIMITED BY SIZE INTO SSA2                                     
157400     MOVE '  GE' TO GODK-STATUSKODER                                      
157500     CALL CBLTDLI USING GHU ARTS-PCB WLARTS11 SSA1 SSA2                   
157600     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
157700     PERFORM IMS-STATUSKONTROLL                                           
157800     .                                                                    
157900     SKIP3                                                                
158000 IMS-REPL-WLARTS11 SECTION.                                               
158100                                                                          
158200     MOVE '  ' TO GODK-STATUSKODER                                        
158300     CALL CBLTDLI USING REPL ARTS-PCB WLARTS11                            
158400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
158500     PERFORM IMS-STATUSKONTROLL                                           
158600     .                                                                    
158700     EJECT                                                                
158800 IMS-GHU-WDK712 SECTION.                                                  
158900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
159000     DELIMITED BY SIZE INTO SSA1                                          
159100     STRING 'WDK712  (IDLAND   =' W-IDLANDX2-X ')'                        
159200     DELIMITED  BY SIZE INTO SSA2                                         
159300     MOVE '  GE' TO GODK-STATUSKODER                                      
159400     CALL CBLTDLI USING GHU WDK712-PCB WDK712 SSA1 SSA2                   
159500     MOVE WDK712-STATUS-CODE TO STATUS-WS                                 
159600     PERFORM IMS-STATUSKONTROLL                                           
159700     .                                                                    
159800     SKIP2                                                                
159900 IMS-REPL-WDK712 SECTION.                                                 
160000     MOVE '    ' TO GODK-STATUSKODER                                      
160100     CALL CBLTDLI USING REPL WDK712-PCB WDK712                            
160200     MOVE WDK712-STATUS-CODE TO STATUS-WS                                 
160300     PERFORM IMS-STATUSKONTROLL                                           
160400     .                                                                    
160500     EJECT                                                                
160600 IMS-GU-WLBENA11 SECTION.                                                 
160700                                                                          
160800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
160900     DELIMITED BY SIZE INTO SSA1                                          
161000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X  ')'                        
161100     DELIMITED BY SIZE INTO SSA2                                          
161200     MOVE '  GE' TO GODK-STATUSKODER                                      
161300     CALL CBLTDLI USING GU BEN-PCB BEN-WLBENA11 SSA1 SSA2                 
161400     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
161500     PERFORM IMS-STATUSKONTROLL                                           
161600     .                                                                    
161700     SKIP3                                                                
161800 IMS-GU-WLLEVA01 SECTION.                                                 
161900                                                                          
162000     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
162100     DELIMITED BY SIZE INTO SSA1                                          
162200     MOVE '  GE' TO GODK-STATUSKODER                                      
162300     CALL CBLTDLI USING GU LEV-PCB WLLEVA01 SSA1                          
162400     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
162500     PERFORM IMS-STATUSKONTROLL                                           
162600     .                                                                    
162700     SKIP3                                                                
162800 IMS-GNP-WLLEVA11 SECTION.                                                
162900                                                                          
163000     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
163100     DELIMITED BY SIZE INTO SSA1                                          
163200     MOVE '  GE' TO GODK-STATUSKODER                                      
163300     CALL CBLTDLI USING GNP LEV-PCB LEV-WLLEVA11 SSA1                     
163400     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
163500     PERFORM IMS-STATUSKONTROLL                                           
163600     .                                                                    
163700     EJECT                                                                
163800 IMS-GU-WDB601    SECTION.                                                
163900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
164000          DELIMITED BY SIZE INTO SSA1                                     
164100     MOVE '  GE' TO GODK-STATUSKODER                                      
164200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
164300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
164400     PERFORM IMS-STATUSKONTROLL                                           
164500     .                                                                    
164600     SKIP3                                                                
164700 IMS-STATUSKONTROLL SECTION.                                              
164800                                                                          
164900     SET STATUS-IX TO 1                                                   
165000     SEARCH GODK-STATUS AT END CALL FELLOG                                
165100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
165200     END-SEARCH                                                           
165300     .                                                                    
165400                                                                          
165500*    -COPY WY2000P1                                                       
165600*    -COPY WY2000P9                                                       
