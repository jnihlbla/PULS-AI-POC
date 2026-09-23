000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1051500.                                                
000300 AUTHOR.         KJELL ANDRE.                                             
000400 DATE-WRITTEN.   94/03/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        UNDERHÅLL AV VADIS-SÖKBEGREPP PER AVSNITT OCH KOLUMN             
001000*        I EN KATALOG.                                                    
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLKATH (WDN5)                              
001300*        PROGRAMMET LÄSER      WDGX1212, (WDR2)                           
001400*        PROGRAMMET LÄSER      WDGX1214, (WDR2)                           
001500*        PROGRAMMET LÄSER      WDGX1216, (WDR2)                           
001600*        PROGRAMMET LÄSER      WDGX1218, (WDR2)                           
001700*        PROGRAMMET LÄSER      WDP7 (USER)                                
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W1T515                                              
002100*        MID:         W1I51501                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W1O51501                                            
002500*                                                                         
002600*    ÄNDRING                                                              
002700*        VID UPPDATERING AV VARIANTRADER KAN FELAKTIGT TAS                
002800*        EN BLNK INMATAD PUBKODS-NYCKEL SOM NYCKEL-VÄRDE TILL             
002900*        BAS-SEGMENTET WDN513                                             
003000*                                                                         
003100*        FEB 2000: Bytt segmentnamn på WDR2                               
003200*                                                                         
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W1051500'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  OCH                         PIC X       VALUE '&'.                   
004800 77  ELLER                       PIC X       VALUE '!'.                   
004900 77  COMMAND-CODE                PIC X       VALUE '-'.                   
005000                                                                          
005100*    --- FÄLT FÖR KONTROLL AV ÅRTAL                                       
005200 01  DATUM.                                                               
005300     03 DAGENS-DATUM.                                                     
005400         05 DAGENS-AAAA          PIC 9(4).                                
005500         05 DAGENS-MAANAD        PIC 9(2).                                
005600         05 DAGENS-DAG           PIC 9(2).                                
005700                                                                          
005800     03 NAESTA-AAR               PIC 9(4).                                
005900                                                                          
006000     03 NAESTA-AAR-X REDEFINES NAESTA-AAR.                                
006100         05 NAESTA-AAR-SEKELDEL  PIC 9(2).                                
006200         05 NAESTA-AAR-ENTALDEL  PIC 9(2).                                
006300                                                                          
006400     03 2-AAR-FRAM               PIC 9(4).                                
006500     03 2-AAR-FRAM-X REDEFINES 2-AAR-FRAM.                                
006600         05 2-AAR-FRAM-SEKELDEL  PIC 9(2).                                
006700         05 2-AAR-FRAM-ENTALDEL  PIC 9(2).                                
006800                                                                          
006900*    --- FÄLT FÖR KONTROLL AV PUBKOD                                      
007000 01  WS-RAETT-PUBKOD             PIC X       VALUE 'J'.                   
007100                                                                          
007200 01  DAGENS-VECKAS-PUB.                                                   
007300     03 DAGENS-VECKAS-PUB-AARDEL PIC 9(4)    VALUE ZERO.                  
007400     03 DAGENS-VECKAS-PUB-VVDEL  PIC 9(2)    VALUE ZERO.                  
007500                                                                          
007600 01  WS-KDCATPUB-R-AVV           PIC X(3)    VALUE SPACE.                 
007700 01  WS-KDCATPUB-AAAAVV          PIC X(6)    VALUE SPACE.                 
007800                                                                          
007900 01  WS-GILTIGA-AAR.                                                      
008000   03 WS-TIAAAA                  PIC 9(4)    VALUE ZERO                   
008100                                 OCCURS 4.                                
008200                                                                          
008300*    --- INDEX FÖR BLÄDDRINGSRADER                                        
008400 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
008500 77  MAX-INDX                    PIC S9(4)   VALUE +10  COMP SYNC.        
008600                                                                          
008700 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
008800 77  Y2K-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
008900                                                                          
009000*    --- MAX-MOD-LAENGD = MOD-LÄNGD + 4. RÄTT VÄRDE SÄTTS I A-INIT        
009100 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +0   COMP SYNC.        
009200                                                                          
009300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009400 77  WS-IDCATNR                  PIC X(5)    VALUE SPACE.                 
009500 77  WS-IDCATGRP                 PIC X(2)    VALUE SPACE.                 
009600 77  WS-IDCATAVS                 PIC X(4)    VALUE SPACE.                 
009700 77  WS-KDCATPUB-FOM-SOEK        PIC X(6)    VALUE LOW-VALUE.             
009800 77  WS-KDCATPUB-TOM             PIC X(6)    VALUE HIGH-VALUE.            
009900                                                                          
010000 01  FILLER                      PIC X(16) VALUE 'SPAR-MSGI'.             
010100*    --- OBS 4-STÄLLIGT RADNR PÅ SKÄRMEN, 5-STÄLLIGT I BASEN              
010200*    --- IDRADN-STA ÄR SPARAT VÄRDE PÅ IDRADN-ENTER                       
010300 01  WS-IDRADN-STA               PIC 9(4)    VALUE ZERO.                  
010400*    --- IDRADN-SLU ÄR SPARAT VÄRDE PÅ SENAST NYSKAPADE RAD               
010500 01  WS-IDRADN-SLU               PIC 9(4)    VALUE ZERO.                  
010600                                                                          
010700 01  SPAR-AREA-MSGI.                                                      
010800     03 SPAR-IDTRANS             PIC X(4)    VALUE '1515'.                
010900     03 FILLER                   PIC X       VALUE SPACE.                 
011000     03 SPAR-IDRADN-ENTER        PIC 9(4)    VALUE ZERO.                  
011100     03 FILLER                   PIC X       VALUE SPACE.                 
011200     03 SPAR-IDRADN-NEXT         PIC 9(4)    VALUE ZERO.                  
011300                                                                          
011400 01  WS-MFSKOD.                                                           
011500     03  FILLER                  PIC XX.                                  
011600                                                                          
011700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
011800     88  INDATA-OK                           VALUE 'J'.                   
011900     88  INDATA-FEL                          VALUE 'N'.                   
012000                                                                          
012100 77  KOMBINATION-SW              PIC X       VALUE 'J'.                   
012200     88  KOMBINATION-OK                      VALUE 'J'.                   
012300     88  KOMBINATION-FEL                     VALUE 'N'.                   
012400                                                                          
012500 77  INTERVALL-SW                PIC X       VALUE 'J'.                   
012600     88  INTERVALL-OK                        VALUE 'J'.                   
012700     88  INTERVALL-FEL                       VALUE 'N'.                   
012800                                                                          
012900                                                                          
013000 77  RADNR-SW                    PIC X       VALUE 'J'.                   
013100     88  RADNR-OK                            VALUE 'J'.                   
013200     88  RADNR-OVERFLOW                      VALUE 'N'.                   
013300                                                                          
013400 77  INPUT-FINNS-SW              PIC X       VALUE 'J'.                   
013500     88  INPUT-FINNS                         VALUE 'J'.                   
013600     88  INPUT-SAKNAS                        VALUE 'N'.                   
013700                                                                          
013800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013900     88  NYCKLAR-OK                          VALUE 'J'.                   
014000     88  NYCKLAR-FEL                         VALUE 'N'.                   
014100                                                                          
014200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014300     88  EGEN-MID                            VALUE '1515'.                
014400     88  GODK-MID                            VALUE '1511' '1512'          
014500                                                   '1513' '1514'          
014600                                                   '1516'                 
014700                                                   '1518' '1519'.         
014800     88  HELP-MID                            VALUE '0551'.                
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16) VALUE 'RAD-SPAR'.              
015100*    --- FÄLT FÖR ATT SPARA FÖREGÅENDE RADS VÄRDEN.                       
015200*    --- ANVÄNDS DÅ MAN SKRIVER '=' I NÅGOT FÄLT FÖR ATT                  
015300*    --- FÅ SAMMA VÄRDEN PÅ FLERA RADER.                                  
015400 01  SPAR-VAERDEN.                                                        
015500     03  SPAR-IDMODELL           PIC X(3)    VALUE SPACE.                 
015600     03  SPAR-TIMODAAR-STA       PIC 9(4)    VALUE ZERO.                  
015700     03  SPAR-TIMODAAR-STO       PIC 9(4)    VALUE ZERO.                  
015800     03  SPAR-IDVARIANT          PIC X(15)   VALUE SPACE.                 
015900     03  SPAR-IDVARIANT-2        PIC X(15)   VALUE SPACE.                 
016000     03  SPAR-KDCHATYP           PIC 9(1)    VALUE ZERO.                  
016100     03  SPAR-IDCHASSI-STA       PIC 9(6)    VALUE ZERO.                  
016200     03  SPAR-IDCHASSI-STO       PIC 9(6)    VALUE ZERO.                  
016300     EJECT                                                                
016400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016500 01  GENERELLA-SUBPROGRAM.                                                
016600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017100     EJECT                                                                
017200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017300*01 -COPY WMEDAREA                                                        
017400     SKIP3                                                                
017500 01  MESSAGE-CODES.                                                       
017600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
017700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
017800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
017900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
018000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
018100     03  ERR-SAKNAS-I-BAS        PIC X(3)    VALUE '010'.                 
018200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
018300     03  ERR-FEL-RADNR           PIC X(3)    VALUE '014'.                 
018400     03  ERR-ALREADY-LAST-PAGE   PIC X(3)    VALUE '115'.                 
018500     03  ERR-INVALID-COMBINATION PIC X(3)    VALUE '238'.                 
018600     03  ERR-AVSNITT-SAKNAS      PIC X(3)    VALUE '239'.                 
018700     03  ERR-FOM-STOERRE-TOM     PIC X(3)    VALUE '240'.                 
018800     03  ERR-SAMMA-KONFIGTYP     PIC X(3)    VALUE '241'.                 
018900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
019000     03  ERR-PUB-CODE-INPUT      PIC X(3)    VALUE '282'.                 
019100     03  FRAGA-UPPD-AVS          PIC X(3)    VALUE '242'.                 
019200     EJECT                                                                
019300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
019400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
019500     SKIP3                                                                
019600*01 -COPY WMSGINIT                                                        
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
019900*01  -COPY WDATAREA                                                       
020000     EJECT                                                                
020100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020300     SKIP3                                                                
020400*01  MID -COPY W1I51501                                                   
020500     EJECT                                                                
020600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020700     SKIP3                                                                
020800*01  -COPY WMSGAREA                                                       
020900     EJECT                                                                
021000     03  MOD REDEFINES MSG-AREA.                                          
021100*      05  -COPY W1O51501                                                 
021200     EJECT                                                                
021300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021400     SKIP3                                                                
021500*01  -COPY WMFSAREA                                                       
021600     EJECT                                                                
021700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021800*                                                                         
021900     EJECT                                                                
022000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022100     SKIP3                                                                
022200 01  NYCKLAR-TILL-DLI.                                                    
022300     03  W-WDN501KY-X.                                                    
022400         05  W-IDCATNR           PIC 9(5)   VALUE ZERO.                   
022500         05  W-IDCATGRP          PIC 9(2)   VALUE ZERO.                   
022600         05  W-IDCATAVS          PIC 9(4)   VALUE ZERO.                   
022700                                                                          
022800*    --- OBS 4-STÄLLIGT RADNR PÅ SKÄRMEN, 5-STÄLLIGT I BASEN              
022900                                                                          
023000     03  W-WDN513KY-X.                                                    
023100         05  W-IDRADN-X.                                                  
023200            07  W-IDRADN         PIC S9(5)   VALUE ZERO COMP-3.           
023300         05  W-KDCATPUB-FOM      PIC X(6)    VALUE LOW-VALUE.             
023400                                                                          
023500     03  W-WDGXKEY-1211-X.                                                
023600         05  W-IDHTYP-1211       PIC X(4)    VALUE '1211'.                
023700         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
023800     03  W-IDMODELL-1212-X.                                               
023900         05  W-IDMODELL-1212     PIC X(3)    VALUE SPACE.                 
024000                                                                          
024100     03  W-WDGXKEY-1213-X.                                                
024200         05  W-IDHTYP-1213       PIC X(4)    VALUE '1213'.                
024300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
024400     03  W-IDVARIANT-1214-X.                                              
024500         05  W-IDVARIANT-1214    PIC X(15)   VALUE SPACE.                 
024600                                                                          
024700     03  W-WDGXKEY-1215-X.                                                
024800         05  W-IDHTYP-1215       PIC X(4)    VALUE '1215'.                
024900         05  W-IDMODELL-1215     PIC X(3)    VALUE SPACE.                 
025000         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
025100     03  W-IDVARIANT-1216-X .                                             
025200         05  W-IDVARIANT-1216    PIC X(15)   VALUE SPACE.                 
025300                                                                          
025400     03  W-WDGXKEY-1217-X.                                                
025500         05  W-IDHTYP-1217       PIC X(4)    VALUE '1217'.                
025600         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
025700     03  W-WDGXKEY-1218-X.                                                
025800         05  W-IDMODELL-1218     PIC X(3)    VALUE SPACE.                 
025900         05  W-TICHAAAR-1218     PIC 9(4)    VALUE ZERO.                  
026000         05  W-KDCHATYP-1218     PIC 9(1)    VALUE ZERO.                  
026100     SKIP2                                                                
026200*    --- STATUS-KOD FRÅN IMS                                              
026300 01  STATUS-WS                   PIC XX.                                  
026400     88  SEGMENT-FINNS                       VALUE '  '.                  
026500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026700     SKIP2                                                                
026800 01  GODK-STATUSKODER.                                                    
026900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027000     SKIP3                                                                
027100 01  SSA1                        PIC X(64).                               
027200 01  SSA2                        PIC X(64).                               
027300     EJECT                                                                
027400*    --- IMS FUNKTIONSKODER                                               
027500*01  -COPY W0003                                                          
027600     EJECT                                                                
027700*    ---  DLI INPUT-OUTPUT AREA                                           
027800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
027900     SKIP3                                                                
028000 01  DLI-IO-AREA.                                                         
028100     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
028200     SKIP3                                                                
028300     03  WLKATH01 REDEFINES IO-AREA.                                      
028400*        05  -COPY WDN501   -PRE KATH-                                    
028500     EJECT                                                                
028600     03  WLKATH12 REDEFINES IO-AREA.                                      
028700*        05  -COPY WDN512   -PRE KATH-                                    
028800     EJECT                                                                
028900     03  WLKATH13 REDEFINES IO-AREA.                                      
029000*        05  -COPY WDN513   -PRE KATH-                                    
029100     EJECT                                                                
029200     03  WDGX1212 REDEFINES IO-AREA.                                      
029300*        05  -COPY WDGX1212                                               
029400     SKIP3                                                                
029500     03  WDGX1214 REDEFINES IO-AREA.                                      
029600*        05  -COPY WDGX1214                                               
029700     EJECT                                                                
029800     03  WDGX1216 REDEFINES IO-AREA.                                      
029900*        05  -COPY WDGX1216                                               
030000     SKIP3                                                                
030100     03  WDGX1218 REDEFINES IO-AREA.                                      
030200*        05  -COPY WDGX1218                                               
030300     EJECT                                                                
030400*    ---  SPAR-AREA FÖR SEGMENT-DATA                                      
030500 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA  '.         
030600     SKIP3                                                                
030700*01  -COPY WDN513   -PRE SPAR-                                            
030800     SKIP3                                                                
030900 01  SPAR-1214-IDKONFIG          PIC 9(1)    VALUE ZERO.                  
031000     EJECT                                                                
031100 LINKAGE SECTION.                                                         
031200                                                                          
031300*01  -COPY W0009   -PRE MSG-                                              
031400     EJECT                                                                
031500*01  -COPY W0008  -PRE USEA-                                              
031600     05  FILLER                  PIC X.                                   
031700     EJECT                                                                
031800*01  -COPY W0008  -PRE KATH-                                              
031900     05  FILLER                  PIC X.                                   
032000     EJECT                                                                
032100*01  -COPY W0008  -PRE 1212-                                              
032200     05  FILLER                  PIC X.                                   
032300     EJECT                                                                
032400*01  -COPY W0008  -PRE 1214-                                              
032500     05  FILLER                  PIC X.                                   
032600     EJECT                                                                
032700*01  -COPY W0008  -PRE 1216-                                              
032800     05  FILLER                  PIC X.                                   
032900     EJECT                                                                
033000*01  -COPY W0008  -PRE 1218-                                              
033100     05  FILLER                  PIC X.                                   
033200     EJECT                                                                
033300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB KATH-PCB                      
033400                           1212-PCB 1214-PCB 1216-PCB 1218-PCB.           
033500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB KATH-PCB                      
033600                           1212-PCB 1214-PCB 1216-PCB 1218-PCB.           
033700                                                                          
033800     PERFORM IMS-GET-MSG                                                  
033900     IF SEGMENT-FINNS                                                     
034000       PERFORM A-INIT                                                     
034100       PERFORM B-KOLLA-NYCKLAR                                            
034200       PERFORM K-BESTAEM-RAETT-PUBKOD                                     
034300       IF NYCKLAR-OK                                                      
034400         IF MFS-UPDATE                                                    
034500           PERFORM G-KOLLA-INPUT                                          
034600           IF INDATA-OK                                                   
034700             PERFORM H-UPPDATERA                                          
034800           END-IF                                                         
034900         ELSE                                                             
035000           EVALUATE TRUE                                                  
035100           WHEN MFS-FIRST                                                 
035200             PERFORM C-FOERSTA-SIDA                                       
035300           WHEN MFS-NEXT                                                  
035400             PERFORM D-NAESTA-SIDA                                        
035500             PERFORM I-KOLLA-ATT-INGET-INMATAT                            
035600           WHEN OTHER                                                     
035700             PERFORM E-SAMMA-SIDA                                         
035800             PERFORM I-KOLLA-ATT-INGET-INMATAT                            
035900           END-EVALUATE                                                   
036000         END-IF                                                           
036100         IF INDATA-OK                                                     
036200           PERFORM F-LAES-VISA-INFO                                       
036300         END-IF                                                           
036400       END-IF                                                             
036500       COMPUTE MSG-KVLL = LENGTH OF MOD-W1O51501 + 4                      
036600       PERFORM IMS-INSERT-MSG                                             
036700     END-IF                                                               
036800                                                                          
036900     MOVE ZERO TO RETURN-CODE                                             
037000     GOBACK                                                               
037100     .                                                                    
037200     EJECT                                                                
037300 A-INIT SECTION.                                                          
037400                                                                          
037500     IF MSG-DUBBLA-TRANSKODER                                             
037600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I51501                 
037700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
037800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
037900     ELSE                                                                 
038000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I51501                  
038100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
038200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
038300     END-IF                                                               
038400                                                                          
038500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
038600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
038700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
038800                                                                          
038900     MOVE LOW-VALUE  TO MSG-AREA                                          
039000     MOVE 'W1O515N1' TO MFS-IDMOD                                         
039100     MOVE '1515'     TO MOD-IDTRANS                                       
039200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
039300                                                                          
039400     IF EGEN-MID OR HELP-MID                                              
039500       CONTINUE                                                           
039600     ELSE                                                                 
039700       MOVE SPACE TO MFS-KDTRTYP                                          
039800       MOVE '7' TO MFS-IDPFK                                              
039900     END-IF                                                               
040000                                                                          
040100                                                                          
040200     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
040300                                                                          
040400     COMPUTE NAESTA-AAR = DAGENS-AAAA + 1                                 
040500     COMPUTE 2-AAR-FRAM = DAGENS-AAAA + 2                                 
040600                                                                          
040700     COMPUTE WS-TIAAAA(1) = DAGENS-AAAA - 1                               
040800     COMPUTE WS-TIAAAA(2) = DAGENS-AAAA                                   
040900     COMPUTE WS-TIAAAA(3) = DAGENS-AAAA + 1                               
041000     COMPUTE WS-TIAAAA(4) = DAGENS-AAAA + 2                               
041100                                                                          
041200     MOVE 'IDAG' TO DAT-KDDATFORM                                         
041300     CALL WDATKONV USING DAT-KDDATFORM                                    
041400                         DAT-I-TIDATUM                                    
041500                         DAT-O-TIDATUM                                    
041600                         DAT-KDSVAR                                       
041700     IF DAT-KDSVAR-OK                                                     
041800        MOVE DAT-TIAAVV-GRP TO DAGENS-VECKAS-PUB-VVDEL                    
041900        MOVE DAGENS-AAAA    TO DAGENS-VECKAS-PUB-AARDEL                   
042000     END-IF                                                               
042100     .                                                                    
042200     EJECT                                                                
042300 B-KOLLA-NYCKLAR SECTION.                                                 
042400                                                                          
042500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
042600     MOVE '001'             TO MSGI-KDCALL                                
042700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
042800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
042900     MOVE '1515'            TO MSGI-IDTRANS                               
043000     IF GODK-MID                                                          
043100*        -- Ändra tillbaka till -IN när alla GODK-MID har WLUSEA          
043200*        MOVE MID-IDCATNR-IN     TO MSGI-IDCATNR                          
043300*        MOVE MID-IDCATGRP-IN    TO MSGI-IDCATGRP                         
043400*        MOVE MID-IDCATAVS-IN    TO MSGI-IDCATAVS                         
043500         MOVE MID-IDCATNR-UT     TO MSGI-IDCATNR                          
043600         MOVE MID-IDCATGRP-UT    TO MSGI-IDCATGRP                         
043700         MOVE MID-IDCATAVS-UT    TO MSGI-IDCATAVS                         
043800     ELSE                                                                 
043900       IF EGEN-MID                                                        
044000         MOVE MID-IDCATNR-IN     TO MSGI-IDCATNR                          
044100         MOVE MID-IDCATGRP-IN    TO MSGI-IDCATGRP                         
044200         MOVE MID-IDCATAVS-IN    TO MSGI-IDCATAVS                         
044300       END-IF                                                             
044400     END-IF                                                               
044500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
044600                                                                          
044700     MOVE MSGI-SPAR-AREA  TO SPAR-AREA-MSGI                               
044800                                                                          
044900     IF MSGI-IDLAND-SPR = 'SE'                                            
045000       MOVE +1 TO SPRAK-IX                                                
045100       MOVE 'S  ' TO MED-IDSKYLT                                          
045200     ELSE                                                                 
045300       MOVE +2 TO SPRAK-IX                                                
045400       MOVE 'GB ' TO MED-IDSKYLT                                          
045500     END-IF                                                               
045600                                                                          
045700     IF MID-IDCATNR-IN NOT = ALL '+'                                      
045800     OR MID-IDCATGRP-IN NOT = ALL '+'                                     
045900     OR MID-IDCATAVS-IN NOT = ALL '+'                                     
046000       MOVE '7'            TO MFS-IDPFK                                   
046100       MOVE SPACE          TO MFS-KDTRTYP                                 
046200     END-IF                                                               
046300                                                                          
046400     MOVE JA TO NYCKLAR-SW                                                
046500*    -------------------------------------- KONTROLL AV IDCATNR -         
046600     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
046700                                                                          
046800     MOVE MSGI-IDCATNR    TO WS-IDCATNR                                   
046900     INSPECT WS-IDCATNR REPLACING LEADING SPACE BY ZERO                   
047000     IF WS-IDCATNR NUMERIC AND WS-IDCATNR > ZERO                          
047100       MOVE WS-IDCATNR    TO W-IDCATNR                                    
047200     ELSE                                                                 
047300       MOVE NEJ TO NYCKLAR-SW                                             
047400     END-IF                                                               
047500     IF GODK-MID OR EGEN-MID OR NYCKLAR-OK                                
047600       MOVE WS-IDCATNR TO MOD-IDCATNR-UT                                  
047700       INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE             
047800     ELSE                                                                 
047900       MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-UT                             
048000     END-IF                                                               
048100                                                                          
048200*    ------------------------------------- KONTROLL AV IDCATGRP -         
048300     MOVE MFS-RENSA-FAELT TO MOD-IDCATGRP-IN                              
048400                                                                          
048500     MOVE MSGI-IDCATGRP   TO WS-IDCATGRP                                  
048600     INSPECT WS-IDCATGRP REPLACING LEADING SPACE BY ZERO                  
048700     IF WS-IDCATGRP NUMERIC AND WS-IDCATGRP > ZERO                        
048800       MOVE WS-IDCATGRP   TO W-IDCATGRP                                   
048900     ELSE                                                                 
049000       MOVE NEJ TO NYCKLAR-SW                                             
049100     END-IF                                                               
049200     IF GODK-MID OR EGEN-MID OR NYCKLAR-OK                                
049300       MOVE WS-IDCATGRP TO MOD-IDCATGRP-UT                                
049400       INSPECT MOD-IDCATGRP-UT REPLACING LEADING ZERO BY SPACE            
049500     ELSE                                                                 
049600       MOVE MFS-RENSA-FAELT TO MOD-IDCATGRP-UT                            
049700     END-IF                                                               
049800                                                                          
049900                                                                          
050000*    ------------------------------------- KONTROLL AV IDCATAVS -         
050100     MOVE MFS-RENSA-FAELT TO MOD-IDCATAVS-IN                              
050200                                                                          
050300     MOVE MSGI-IDCATAVS   TO WS-IDCATAVS                                  
050400     INSPECT WS-IDCATAVS REPLACING LEADING SPACE BY ZERO                  
050500     IF WS-IDCATAVS NUMERIC AND WS-IDCATAVS > ZERO                        
050600       MOVE WS-IDCATAVS   TO W-IDCATAVS                                   
050700     ELSE                                                                 
050800       MOVE NEJ TO NYCKLAR-SW                                             
050900     END-IF                                                               
051000     IF GODK-MID OR EGEN-MID OR NYCKLAR-OK                                
051100       MOVE WS-IDCATAVS TO MOD-IDCATAVS-UT                                
051200       INSPECT MOD-IDCATAVS-UT REPLACING LEADING ZERO BY SPACE            
051300     ELSE                                                                 
051400       MOVE MFS-RENSA-FAELT TO MOD-IDCATAVS-UT                            
051500     END-IF                                                               
051600                                                                          
051700*    ----------------------------------- KONTROLL AV KDCATPUB-R--         
051800     MOVE MFS-RENSA-FAELT TO MOD-KDCATPUB-R-FOM-IN                        
051900                                                                          
052000     IF MID-KDCATPUB-R-FOM-IN = ALL '+'                                   
052100       IF MID-KDCATPUB-R-FOM-UT  NOT NUMERIC                              
052200         MOVE SPACE                 TO MID-KDCATPUB-R-FOM-UT              
052300       END-IF                                                             
052400       MOVE MID-KDCATPUB-R-FOM-UT TO WS-KDCATPUB-R-AVV                    
052500       PERFORM S50-Y2K-KDCATPUB-R                                         
052600       MOVE WS-KDCATPUB-AAAAVV    TO W-KDCATPUB-FOM                       
052700     ELSE                                                                 
052800       IF MID-KDCATPUB-R-FOM-IN = SPACE                                   
052900       OR MID-KDCATPUB-R-FOM-IN NUMERIC                                   
053000         CONTINUE                                                         
053100       ELSE                                                               
053200         MOVE SPACE TO MID-KDCATPUB-R-FOM-IN                              
053300       END-IF                                                             
053400       MOVE MID-KDCATPUB-R-FOM-IN TO WS-KDCATPUB-R-AVV                    
053500       PERFORM S50-Y2K-KDCATPUB-R                                         
053600       MOVE WS-KDCATPUB-AAAAVV    TO W-KDCATPUB-FOM                       
053700                                                                          
053800       MOVE '7'            TO MFS-IDPFK                                   
053900       MOVE SPACE          TO MFS-KDTRTYP                                 
054000     END-IF                                                               
054100     INSPECT W-KDCATPUB-FOM REPLACING LEADING SPACE BY LOW-VALUE          
054200                                                                          
054300     IF ( W-KDCATPUB-FOM NUMERIC AND W-KDCATPUB-FOM > ZERO )              
054400     OR ( W-KDCATPUB-FOM = LOW-VALUE )                                    
054500       CONTINUE                                                           
054600     ELSE                                                                 
054700       MOVE NEJ TO NYCKLAR-SW                                             
054800       MOVE ERR-PUB-CODE-INPUT TO MED-IDMFSMED                            
054900       CALL WMEDKONV USING MED-WMEDAREA                                   
055000       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
055100     END-IF                                                               
055200                                                                          
055300     IF GODK-MID OR EGEN-MID OR NYCKLAR-OK                                
055400       MOVE W-KDCATPUB-FOM (4:3) TO MOD-KDCATPUB-R-FOM-UT                 
055500     ELSE                                                                 
055600       MOVE MFS-RENSA-FAELT TO MOD-KDCATPUB-R-FOM-UT                      
055700     END-IF                                                               
055800*    ---------------------------------- EJ KONTROLL AV IDSKYLT --         
055900     MOVE MFS-RENSA-FAELT TO MOD-IDSKYLT-IN                               
056000     MOVE MID-IDSKYLT-UT  TO MOD-IDSKYLT-UT                               
056100                                                                          
056200*    ---------------------------------- EJ KONTROLL AV IDCATRAD -         
056300     MOVE MFS-RENSA-FAELT TO MOD-IDCATRAD-IN                              
056400     MOVE MID-IDCATRAD-UT TO MOD-IDCATRAD-UT                              
056500                                                                          
056600     IF NYCKLAR-FEL                                                       
056700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
056800       CALL WMEDKONV USING MED-WMEDAREA                                   
056900       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
057000       PERFORM MFS-RENSA-FAELT-IN                                         
057100       PERFORM MFS-RENSA-FAELT-UT                                         
057200     END-IF                                                               
057300     .                                                                    
057400     EJECT                                                                
057500 C-FOERSTA-SIDA SECTION.                                                  
057600                                                                          
057700     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
057800     CALL WMEDKONV USING MED-WMEDAREA                                     
057900     MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                    
058000                                                                          
058100*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
058200     MOVE ZERO  TO W-IDRADN                                               
058300     PERFORM MFS-RENSA-FAELT-IN                                           
058400     .                                                                    
058500     EJECT                                                                
058600 D-NAESTA-SIDA SECTION.                                                   
058700                                                                          
058800     IF SPAR-IDRADN-NEXT NUMERIC                                          
058900       MOVE SPAR-IDRADN-NEXT TO WS-IDRADN-STA                             
059000     ELSE                                                                 
059100       MOVE ZERO TO WS-IDRADN-STA                                         
059200     END-IF                                                               
059300     PERFORM MFS-RENSA-FAELT-IN                                           
059400                                                                          
059500     IF MID-IDRADNR(MAX-INDX) = ZERO                                      
059600       MOVE ERR-ALREADY-LAST-PAGE TO MED-IDMFSINF                         
059700       CALL WMEDKONV USING MED-WMEDAREA                                   
059800       MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                  
059900       MOVE NEJ TO INDATA-SW                                              
060000       PERFORM MFS-ROER-EJ-FAELT-UT                                       
060100     END-IF                                                               
060200     .                                                                    
060300     EJECT                                                                
060400 E-SAMMA-SIDA SECTION.                                                    
060500                                                                          
060600     IF EGEN-MID OR HELP-MID                                              
060700       IF SPAR-IDRADN-ENTER NUMERIC                                       
060800         MOVE SPAR-IDRADN-ENTER TO WS-IDRADN-STA                          
060900       ELSE                                                               
061000         MOVE ZERO TO WS-IDRADN-STA                                       
061100       END-IF                                                             
061200     ELSE                                                                 
061300       PERFORM MFS-RENSA-FAELT-IN                                         
061400     END-IF                                                               
061500     .                                                                    
061600     EJECT                                                                
061700 F-LAES-VISA-INFO SECTION.                                                
061800     SKIP2                                                                
061900     PERFORM FA-LAES-VISA-GRUNDDATA                                       
062000                                                                          
062100     IF SEGMENT-SAKNAS                                                    
062200        MOVE ERR-AVSNITT-SAKNAS TO MED-IDMFSFEL                           
062300        CALL WMEDKONV USING MED-WMEDAREA                                  
062400        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
062500        PERFORM MFS-RENSA-FAELT-UT                                        
062600     ELSE                                                                 
062700       MOVE +1 TO INDX                                                    
062800       MOVE WS-IDRADN-STA TO W-IDRADN                                     
062900       PERFORM IMS-GETF-KATH-VADIS                                        
063000       IF SEGMENT-FINNS                                                   
063100         PERFORM FB-VISA-RADDATA                                          
063200         MOVE KATH-VADIS-IDRADN TO SPAR-IDRADN-ENTER                      
063300       ELSE                                                               
063400         MOVE MFS-RENSA-FAELT    TO SPAR-IDRADN-ENTER                     
063500         MOVE ZERO               TO W-IDRADN                              
063600       END-IF                                                             
063700       PERFORM UNTIL INDX > MAX-INDX                                      
063800         IF SEGMENT-FINNS                                                 
063900           PERFORM FB-VISA-RADDATA                                        
064000           PERFORM IMS-GETN-KATH-VADIS                                    
064100         ELSE                                                             
064200           PERFORM MFS-RENSA-RADDATA-UT                                   
064300         END-IF                                                           
064400         ADD 1 TO INDX                                                    
064500       END-PERFORM                                                        
064600                                                                          
064700       IF SEGMENT-FINNS                                                   
064800         MOVE KATH-VADIS-IDRADN    TO SPAR-IDRADN-NEXT                    
064900         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
065000         CALL WMEDKONV USING MED-WMEDAREA                                 
065100         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
065200       ELSE                                                               
065300         MOVE KATH-VADIS-IDRADN    TO SPAR-IDRADN-NEXT                    
065400       END-IF                                                             
065500     END-IF                                                               
065600                                                                          
065700     MOVE '002'      TO MSGI-KDCALL                                       
065800     MOVE '1515'     TO SPAR-IDTRANS                                      
065900     MOVE SPAR-AREA-MSGI TO MSGI-SPAR-AREA                                
066000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
066100     .                                                                    
066200     EJECT                                                                
066300 FA-LAES-VISA-GRUNDDATA SECTION.                                          
066400                                                                          
066500     PERFORM IMS-GET-KATH-AVS                                             
066600                                                                          
066700     IF SEGMENT-FINNS                                                     
066800       IF KATH-AVS-FLAVSTVAD = LOW-VALUE                                  
066900         MOVE JA                 TO MOD-FLAVSTVAD                         
067000       ELSE                                                               
067100         MOVE KATH-AVS-FLAVSTVAD TO MOD-FLAVSTVAD                         
067200       END-IF                                                             
067300     END-IF                                                               
067400     .                                                                    
067500     EJECT                                                                
067600 FB-VISA-RADDATA SECTION.                                                 
067700                                                                          
067800     MOVE KATH-VADIS-IDRADN TO MOD-IDRADNR(INDX)                          
067900                                                                          
068000     MOVE KATH-VADIS-IDKOL         TO MOD-IDKOL (INDX)                    
068100     IF KATH-VADIS-FLEXCL = JA                                            
068200       MOVE 'X'   TO MOD-FLEXCL (INDX)                                    
068300     ELSE                                                                 
068400       MOVE SPACE TO MOD-FLEXCL (INDX)                                    
068500     END-IF                                                               
068600     MOVE KATH-VADIS-IDMODELL      TO MOD-IDMODELL (INDX)                 
068700                                                                          
068800     IF KATH-VADIS-TIMODAAR-STA = ZERO                                    
068900       MOVE MFS-RENSA-FAELT         TO MOD-TIMODAAR-STA (INDX)            
069000     ELSE                                                                 
069100       MOVE KATH-VADIS-TIMODAAR-STA TO MOD-TIMODAAR-STA (INDX)            
069200     END-IF                                                               
069300                                                                          
069400     IF KATH-VADIS-TIMODAAR-STO = ZERO                                    
069500       MOVE MFS-RENSA-FAELT         TO MOD-TIMODAAR-STO (INDX)            
069600     ELSE                                                                 
069700       MOVE KATH-VADIS-TIMODAAR-STO TO MOD-TIMODAAR-STO (INDX)            
069800     END-IF                                                               
069900                                                                          
070000     MOVE KATH-VADIS-IDVARIANT     TO MOD-IDVARIANT     (INDX)            
070100     MOVE KATH-VADIS-IDVARIANT-2   TO MOD-IDVARIANT-2   (INDX)            
070200                                                                          
070300     IF KATH-VADIS-KDCHATYP = ZERO                                        
070400       MOVE MFS-RENSA-FAELT         TO MOD-KDCHATYP     (INDX)            
070500     ELSE                                                                 
070600       MOVE KATH-VADIS-KDCHATYP     TO MOD-KDCHATYP     (INDX)            
070700     END-IF                                                               
070800                                                                          
070900     IF KATH-VADIS-IDCHASSI-STA = ZERO                                    
071000       MOVE MFS-RENSA-FAELT         TO MOD-IDCHASSI-STA (INDX)            
071100     ELSE                                                                 
071200       MOVE KATH-VADIS-IDCHASSI-STA TO MOD-IDCHASSI-STA (INDX)            
071300     END-IF                                                               
071400                                                                          
071500     IF KATH-VADIS-IDCHASSI-STO = ZERO                                    
071600       MOVE MFS-RENSA-FAELT         TO MOD-IDCHASSI-STO (INDX)            
071700     ELSE                                                                 
071800       MOVE KATH-VADIS-IDCHASSI-STO TO MOD-IDCHASSI-STO (INDX)            
071900     END-IF                                                               
072000     .                                                                    
072100     EJECT                                                                
072200 G-KOLLA-INPUT SECTION.                                                   
072300                                                                          
072400     IF SPAR-IDRADN-ENTER NUMERIC                                         
072500       MOVE SPAR-IDRADN-ENTER TO WS-IDRADN-STA                            
072600     ELSE                                                                 
072700       MOVE ZERO TO WS-IDRADN-STA                                         
072800     END-IF                                                               
072900                                                                          
073000     MOVE JA TO INDATA-SW                                                 
073100                                                                          
073200     PERFORM S10-KOLLA-OM-INPUT                                           
073300     IF INPUT-SAKNAS                                                      
073400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
073500       CALL WMEDKONV USING MED-WMEDAREA                                   
073600       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
073700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
073800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
073900       MOVE NEJ TO INDATA-SW                                              
074000     ELSE                                                                 
074100                                                                          
074200*      -- PREPARE   FÖR ATT LÄSA IN ALLT DATA IGEN OM NÅGOT ÄR FEL        
074300       PERFORM MFS-LAESIN-MODIF-DATA-IGEN                                 
074400                                                                          
074500       PERFORM GA-KOLLA-FAST-INPUT                                        
074600                                                                          
074700       MOVE 1 TO INDX                                                     
074800       PERFORM UNTIL INDX > MAX-INDX                                      
074900         PERFORM S11-KOLLA-OM-RADINPUT                                    
075000         IF INPUT-FINNS                                                   
075100*          --- FORMELLA FÄLTVISA KONTROLLER                               
075200           PERFORM GB-KOLLA-RADINPUT-FORMELLT                             
075300         END-IF                                                           
075400         ADD 1 TO INDX                                                    
075500       END-PERFORM                                                        
075600                                                                          
075700       IF INDATA-FEL                                                      
075800         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
075900         CALL WMEDKONV USING MED-WMEDAREA                                 
076000         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
076100         PERFORM MFS-ROER-EJ-FAELT-UT                                     
076200         PERFORM MFS-ROER-EJ-FAELT-IN                                     
076300       ELSE                                                               
076400                                                                          
076500*        --- GÖR MER NOGRANN KONTROLL AV RADDATA                          
076600         MOVE 1 TO INDX                                                   
076700         PERFORM UNTIL INDX > MAX-INDX                                    
076800           PERFORM S11-KOLLA-OM-RADINPUT                                  
076900           IF INPUT-FINNS                                                 
077000             PERFORM GC-KOLLA-RADINPUT-KOPPLAT                            
077100             IF KOMBINATION-OK                                            
077200               PERFORM GD-KOLLA-RADINPUT-MOT-WDR2                         
077300             END-IF                                                       
077400           END-IF                                                         
077500           ADD 1 TO INDX                                                  
077600         END-PERFORM                                                      
077700                                                                          
077800         IF INDATA-FEL                                                    
077900*          --- FEL-NR HAR SATTS I GC/GD-SEKTIONEN                         
078000           CALL WMEDKONV USING MED-WMEDAREA                               
078100           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
078200           PERFORM MFS-ROER-EJ-FAELT-UT                                   
078300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
078400         END-IF                                                           
078500                                                                          
078600       END-IF                                                             
078700     END-IF                                                               
078800     .                                                                    
078900     EJECT                                                                
079000 GA-KOLLA-FAST-INPUT SECTION.                                             
079100                                                                          
079200*    --- KOLLA INPUT PÅ ICKE BLÄDDRINGSBAR DEL AV SKÄRMEN                 
079300                                                                          
079400     IF MID-FLAVSTVAD NOT = ALL '+'                                       
079500       IF MID-FLAVSTVAD = 'J' OR 'N' OR 'X'                               
079600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAVSTVAD-ATTR                  
079700         IF MID-FLAVSTVAD  = 'X'                                          
079800           MOVE 'J' TO MID-FLAVSTVAD                                      
079900         END-IF                                                           
080000       ELSE                                                               
080100         MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLAVSTVAD-ATTR                   
080200         MOVE NEJ TO INDATA-SW                                            
080300       END-IF                                                             
080400     END-IF                                                               
080500     .                                                                    
080600     EJECT                                                                
080700 GB-KOLLA-RADINPUT-FORMELLT  SECTION.                                     
080800                                                                          
080900*--- FORMELLA FÄLTVISA KONTROLLER AV FÄLT I BLÄDDER-RAD:                  
081000                                                                          
081100     IF MID-IDKOL (INDX)    NOT = ALL '+'                                 
081200       IF MID-IDKOL (INDX) = SPACE                                        
081300       OR 'A' OR 'B' OR 'C' OR 'D' OR 'E' OR '/'                          
081400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKOL-ATTR (INDX)               
081500       ELSE                                                               
081600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDKOL-ATTR (INDX)               
081700         MOVE NEJ TO INDATA-SW                                            
081800       END-IF                                                             
081900     END-IF                                                               
082000                                                                          
082100*--- "KOMMANDO" FÖR ATT RENSA HELA RADEN:                                 
082200     IF MID-IDKOL (INDX) = '/'                                            
082300       MOVE SPACE    TO   MID-IDKOL (INDX)                                
082400       MOVE NEJ      TO   MID-FLEXCL (INDX)                               
082500       MOVE SPACE    TO   MID-IDMODELL (INDX)                             
082600       MOVE ZERO     TO   MID-TIMODAAR-STA (INDX)                         
082700       MOVE ZERO     TO   MID-TIMODAAR-STO (INDX)                         
082800       MOVE SPACE    TO   MID-IDVARIANT (INDX)                            
082900       MOVE SPACE    TO   MID-IDVARIANT-2 (INDX)                          
083000       MOVE ZERO     TO   MID-KDCHATYP    (INDX)                          
083100       MOVE ZERO     TO   MID-IDCHASSI-STA (INDX)                         
083200       MOVE ZERO     TO   MID-IDCHASSI-STO (INDX)                         
083300     END-IF                                                               
083400                                                                          
083500                                                                          
083600     IF MID-FLEXCL (INDX) NOT = ALL '+'                                   
083700       IF MID-FLEXCL (INDX) = SPACE OR 'J' OR 'X' OR 'N' OR 'E'           
083800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCL-ATTR (INDX)              
083900         IF MID-FLEXCL (INDX) = SPACE                                     
084000           MOVE 'N' TO MID-FLEXCL (INDX)                                  
084100         END-IF                                                           
084200         IF MID-FLEXCL (INDX) = 'X' OR 'E'                                
084300           MOVE 'J' TO MID-FLEXCL (INDX)                                  
084400         END-IF                                                           
084500       ELSE                                                               
084600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLEXCL-ATTR (INDX)              
084700         MOVE NEJ TO INDATA-SW                                            
084800       END-IF                                                             
084900     END-IF                                                               
085000                                                                          
085100                                                                          
085200     IF MID-IDMODELL (INDX) = '='                                         
085300       MOVE SPAR-IDMODELL TO MID-IDMODELL (INDX)                          
085400     END-IF                                                               
085500     IF MID-IDMODELL (INDX) NOT = ALL '+'                                 
085600       IF MID-IDMODELL (INDX) = SPACE                                     
085700       OR MID-IDMODELL (INDX) NUMERIC                                     
085800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDMODELL-ATTR (INDX)            
085900       ELSE                                                               
086000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDMODELL-ATTR (INDX)            
086100         MOVE NEJ TO INDATA-SW                                            
086200       END-IF                                                             
086300     END-IF                                                               
086400     MOVE MID-IDMODELL (INDX) TO SPAR-IDMODELL                            
086500                                                                          
086600                                                                          
086700     IF MID-TIMODAAR-STA (INDX) = '='                                     
086800       MOVE SPAR-TIMODAAR-STA TO MID-TIMODAAR-STA (INDX)                  
086900     END-IF                                                               
087000     IF MID-TIMODAAR-STA (INDX) NOT = ALL '+'                             
087100       IF MID-TIMODAAR-STA (INDX) = SPACE                                 
087200       OR '000 ' OR '00  ' OR '0   '                                      
087300         MOVE ZERO TO MID-TIMODAAR-STA (INDX)                             
087400       END-IF                                                             
087500       IF MID-TIMODAAR-STA (INDX) NUMERIC                                 
087600       AND (MID-TIMODAAR-STA (INDX) = ZERO                                
087700       OR (MID-TIMODAAR-STA (INDX) >= 1974 AND <= 2-AAR-FRAM))            
087800         MOVE MFS-NUM-FAELT-RAETT TO MOD-TIMODAAR-STA-ATTR (INDX)         
087900       ELSE                                                               
088000         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIMODAAR-STA-ATTR (INDX)         
088100         MOVE NEJ TO INDATA-SW                                            
088200       END-IF                                                             
088300     END-IF                                                               
088400     MOVE MID-TIMODAAR-STA (INDX) TO SPAR-TIMODAAR-STA                    
088500                                                                          
088600                                                                          
088700     IF MID-TIMODAAR-STO (INDX) = '='                                     
088800       MOVE SPAR-TIMODAAR-STO TO MID-TIMODAAR-STO (INDX)                  
088900     END-IF                                                               
089000     IF MID-TIMODAAR-STO (INDX) NOT = ALL '+'                             
089100       IF MID-TIMODAAR-STO (INDX) = SPACE                                 
089200       OR '000 ' OR '00  ' OR '0   '                                      
089300         MOVE ZERO TO MID-TIMODAAR-STO (INDX)                             
089400       END-IF                                                             
089500       IF MID-TIMODAAR-STO (INDX) NUMERIC                                 
089600       AND (MID-TIMODAAR-STO (INDX) = ZERO                                
089700       OR (MID-TIMODAAR-STO (INDX) >= 1974 AND <= 2-AAR-FRAM))            
089800         MOVE MFS-NUM-FAELT-RAETT TO MOD-TIMODAAR-STO-ATTR (INDX)         
089900       ELSE                                                               
090000         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIMODAAR-STO-ATTR (INDX)         
090100         MOVE NEJ TO INDATA-SW                                            
090200       END-IF                                                             
090300     END-IF                                                               
090400     MOVE MID-TIMODAAR-STO (INDX) TO SPAR-TIMODAAR-STO                    
090500                                                                          
090600                                                                          
090700     IF MID-IDVARIANT (INDX) = '='                                        
090800       MOVE SPAR-IDVARIANT TO MID-IDVARIANT (INDX)                        
090900     END-IF                                                               
091000     IF MID-IDVARIANT (INDX) NOT = ALL '+'                                
091100       IF MID-IDVARIANT (INDX) = SPACE OR NOT = SPACE                     
091200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDVARIANT-ATTR    (INDX)        
091300       ELSE                                                               
091400         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDVARIANT-ATTR    (INDX)        
091500         MOVE NEJ TO INDATA-SW                                            
091600       END-IF                                                             
091700     END-IF                                                               
091800     MOVE MID-IDVARIANT (INDX) TO SPAR-IDVARIANT                          
091900                                                                          
092000                                                                          
092100     IF MID-IDVARIANT-2 (INDX) = '='                                      
092200       MOVE SPAR-IDVARIANT-2 TO MID-IDVARIANT-2 (INDX)                    
092300     END-IF                                                               
092400     IF MID-IDVARIANT-2 (INDX) NOT = ALL '+'                              
092500       IF MID-IDVARIANT-2 (INDX) = SPACE OR NOT = SPACE                   
092600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDVARIANT-2-ATTR  (INDX)        
092700       ELSE                                                               
092800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDVARIANT-2-ATTR  (INDX)        
092900         MOVE NEJ TO INDATA-SW                                            
093000       END-IF                                                             
093100     END-IF                                                               
093200     MOVE MID-IDVARIANT-2 (INDX) TO SPAR-IDVARIANT-2                      
093300                                                                          
093400                                                                          
093500     IF MID-KDCHATYP (INDX) = '='                                         
093600       MOVE SPAR-KDCHATYP TO MID-KDCHATYP (INDX)                          
093700     END-IF                                                               
093800     IF MID-KDCHATYP (INDX) NOT = ALL '+'                                 
093900       IF MID-KDCHATYP (INDX) = SPACE                                     
094000         MOVE ZERO TO MID-KDCHATYP (INDX)                                 
094100       END-IF                                                             
094200       IF MID-KDCHATYP (INDX)  NUMERIC                                    
094300       AND (MID-KDCHATYP (INDX) = ZERO OR '2' OR '3'                      
094400                                       OR '4' OR '5')                     
094500         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDCHATYP-ATTR     (INDX)         
094600       ELSE                                                               
094700         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDCHATYP-ATTR     (INDX)         
094800         MOVE NEJ TO INDATA-SW                                            
094900       END-IF                                                             
095000     END-IF                                                               
095100     MOVE MID-KDCHATYP (INDX) TO SPAR-KDCHATYP                            
095200                                                                          
095300                                                                          
095400     IF MID-IDCHASSI-STA (INDX) = '='                                     
095500       MOVE SPAR-IDCHASSI-STA TO MID-IDCHASSI-STA (INDX)                  
095600     END-IF                                                               
095700     IF MID-IDCHASSI-STA (INDX) NOT = ALL '+'                             
095800       IF MID-IDCHASSI-STA (INDX) = SPACE                                 
095900       OR '00000 ' OR '0000  ' OR '000   '                                
096000       OR '00    ' OR '0     '                                            
096100         MOVE ZERO TO MID-IDCHASSI-STA (INDX)                             
096200       END-IF                                                             
096300       INSPECT MID-IDCHASSI-STA (INDX)                                    
096400               REPLACING LEADING SPACE BY ZERO                            
096500       IF MID-IDCHASSI-STA (INDX)  NUMERIC                                
096600         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCHASSI-STA-ATTR (INDX)         
096700       ELSE                                                               
096800         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDCHASSI-STA-ATTR (INDX)         
096900         MOVE NEJ TO INDATA-SW                                            
097000       END-IF                                                             
097100     END-IF                                                               
097200     MOVE MID-IDCHASSI-STA (INDX) TO SPAR-IDCHASSI-STA                    
097300                                                                          
097400                                                                          
097500     IF MID-IDCHASSI-STO (INDX) = '='                                     
097600       MOVE SPAR-IDCHASSI-STO TO MID-IDCHASSI-STO (INDX)                  
097700     END-IF                                                               
097800     IF MID-IDCHASSI-STO (INDX) NOT = ALL '+'                             
097900       IF MID-IDCHASSI-STO (INDX) = SPACE                                 
098000       OR '00000 ' OR '0000  ' OR '000   '                                
098100       OR '00    ' OR '0     '                                            
098200         MOVE ZERO TO MID-IDCHASSI-STO (INDX)                             
098300       END-IF                                                             
098400       INSPECT MID-IDCHASSI-STO (INDX)                                    
098500               REPLACING LEADING SPACE BY ZERO                            
098600       IF MID-IDCHASSI-STO (INDX)  NUMERIC                                
098700         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCHASSI-STO-ATTR (INDX)         
098800       ELSE                                                               
098900         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDCHASSI-STO-ATTR (INDX)         
099000         MOVE NEJ TO INDATA-SW                                            
099100       END-IF                                                             
099200     END-IF                                                               
099300     MOVE MID-IDCHASSI-STO (INDX) TO SPAR-IDCHASSI-STO                    
099400     .                                                                    
099500     EJECT                                                                
099600 GC-KOLLA-RADINPUT-KOPPLAT  SECTION.                                      
099700                                                                          
099800*--- KOPPLADE KONTROLLER MELLAN ALLA FÄLT I EN BLÄDDRINGSRAD              
099900*--- OBEROENDE AV OM DET ÄR NYINMATAT (RADNR = ZERO) ELLER                
100000*--- BLANDAT NYINMATAT OCH BEFINTLIG RAD I DATABASEN ( > ZERO)            
100100                                                                          
100200*    --- RENSA FELSIGNAL FÖR TIDIGARE RADER                               
100300     MOVE JA TO KOMBINATION-SW                                            
100400     MOVE JA TO INTERVALL-SW                                              
100500                                                                          
100600*    --- HÄMTA ELLER FIXA TILL BEFINTLIGT DATA                            
100700     IF MID-IDRADNR(INDX) = ZERO                                          
100800       MOVE ZERO TO W-IDRADN                                              
100900       PERFORM S12-INIT-KATH-VADIS                                        
101000       SET SEGMENT-FINNS TO TRUE                                          
101100     ELSE                                                                 
101200       MOVE MID-IDRADNR(INDX) TO W-IDRADN                                 
101300       PERFORM IMS-GETU-KATH-VADIS                                        
101400     END-IF                                                               
101500                                                                          
101600     IF SEGMENT-FINNS                                                     
101700*      --- MIXA IHOP NYINMATAT DATA MED BEFINTLIGT OCH KOLLA              
101800       PERFORM S13-MID-TILL-KATH-VADIS                                    
101900       PERFORM GCA-KOLLA-IN-OCH-BEF-DATA                                  
102000     ELSE                                                                 
102100       MOVE NEJ TO INDATA-SW                                              
102200       MOVE ERR-FEL-RADNR TO MED-IDMFSFEL                                 
102300     END-IF                                                               
102400     .                                                                    
102500     EJECT                                                                
102600 GCA-KOLLA-IN-OCH-BEF-DATA SECTION.                                       
102700                                                                          
102800*--- EXKLUDERADE NYCKLAR UTAN VARIANT/MODELL-ANGIVELSE         ---        
102900*--- KOMBINATIONER EJ TILLÅTET - ANGIVET FÄLT TOLKAS NEGATIVT. ---        
103000                                                                          
103100     IF KATH-VADIS-FLEXCL  = JA                                           
103200     AND KATH-VADIS-IDMODELL    = SPACE                                   
103300     AND KATH-VADIS-IDVARIANT = SPACE                                     
103400                                                                          
103500       IF KATH-VADIS-TIMODAAR-STA  NOT = ZERO                             
103600         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIMODAAR-STO-ATTR (INDX)         
103700         MOVE NEJ TO KOMBINATION-SW                                       
103800       END-IF                                                             
103900       IF KATH-VADIS-TIMODAAR-STO  NOT = ZERO                             
104000         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIMODAAR-STO-ATTR (INDX)         
104100         MOVE NEJ TO KOMBINATION-SW                                       
104200       END-IF                                                             
104300       IF KATH-VADIS-IDVARIANT-2  NOT = SPACE                             
104400         MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDVARIANT-2-ATTR (INDX)          
104500         MOVE NEJ TO KOMBINATION-SW                                       
104600       END-IF                                                             
104700       IF KATH-VADIS-KDCHATYP  NOT = ZERO                                 
104800         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDCHATYP-ATTR (INDX)             
104900         MOVE NEJ TO KOMBINATION-SW                                       
105000       END-IF                                                             
105100       IF KATH-VADIS-IDCHASSI-STA  NOT = ZERO                             
105200         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDCHASSI-STA-ATTR (INDX)         
105300         MOVE NEJ TO KOMBINATION-SW                                       
105400       END-IF                                                             
105500       IF KATH-VADIS-IDCHASSI-STO  NOT = ZERO                             
105600         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDCHASSI-STO-ATTR (INDX)         
105700         MOVE NEJ TO KOMBINATION-SW                                       
105800       END-IF                                                             
105900                                                                          
106000     END-IF                                                               
106100                                                                          
106200     EJECT                                                                
106300*--- NORMALA NYCKLAR ELLER EXCL TILLSAMMANS MED VARIANT ELLER  ---        
106400*--- MODELL. I DE SENARE FALLEN TOLKAS VARIANT/MODELL NEGATIVT ---        
106500*--- MEDAN ÅRTAL OCH CHASSINR TOLKAS POSITIVT.                 ---        
106600                                                                          
106700     IF KATH-VADIS-FLEXCL  = NEJ                                          
106800     OR KATH-VADIS-IDMODELL NOT = SPACE                                   
106900     OR KATH-VADIS-IDVARIANT NOT = SPACE                                  
107000                                                                          
107100       IF KATH-VADIS-FLEXCL = JA                                          
107200       AND KATH-VADIS-IDMODELL NOT = SPACE                                
107300       AND KATH-VADIS-IDVARIANT NOT = SPACE                               
107400         MOVE NEJ TO KOMBINATION-SW                                       
107500         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCL-ATTR (INDX)                
107600                                  MOD-IDMODELL-ATTR (INDX)                
107700                               MOD-IDVARIANT-ATTR (INDX)                  
107800       END-IF                                                             
107900                                                                          
108000       IF KATH-VADIS-TIMODAAR-STA  > ZERO                                 
108100       AND KATH-VADIS-TIMODAAR-STO  > ZERO                                
108200         IF KATH-VADIS-TIMODAAR-STA > KATH-VADIS-TIMODAAR-STO             
108300           MOVE NEJ TO INTERVALL-SW                                       
108400           MOVE MFS-NUM-FAELT-FEL TO MOD-TIMODAAR-STA-ATTR (INDX)         
108500                                     MOD-TIMODAAR-STO-ATTR (INDX)         
108600         END-IF                                                           
108700       END-IF                                                             
108800                                                                          
108900       IF KATH-VADIS-KDCHATYP NOT = ZERO                                  
109000         IF  KATH-VADIS-IDCHASSI-STA  = ZERO                              
109100         AND KATH-VADIS-IDCHASSI-STO  = ZERO                              
109200           MOVE NEJ TO KOMBINATION-SW                                     
109300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCHASSI-STA-ATTR (INDX)         
109400                                     MOD-IDCHASSI-STO-ATTR (INDX)         
109500                                     MOD-KDCHATYP-ATTR (INDX)             
109600         END-IF                                                           
109700         IF KATH-VADIS-IDCHASSI-STA  = ZERO                               
109800         AND KATH-VADIS-TIMODAAR-STA NOT  = ZERO                          
109900           MOVE NEJ TO KOMBINATION-SW                                     
110000           MOVE MFS-NUM-FAELT-FEL TO MOD-TIMODAAR-STA-ATTR (INDX)         
110100                                     MOD-IDCHASSI-STA-ATTR (INDX)         
110200         END-IF                                                           
110300*        -- TILLÅT SLUTÅR MEN INGET CHASSINR - SISTA CHASSI PÅ            
110400*        -- ÅRET ANTAS.                                                   
110500*        IF KATH-VADIS-IDCHASSI-STO  = ZERO                               
110600*        AND KATH-VADIS-TIMODAAR-STO NOT = ZERO                           
110700*          MOVE NEJ TO KOMBINATION-SW                                     
110800*          MOVE MFS-NUM-FAELT-FEL TO MOD-TIMODAAR-STO-ATTR (INDX)         
110900*                                    MOD-IDCHASSI-STO-ATTR (INDX)         
111000*        END-IF                                                           
111100       END-IF                                                             
111200                                                                          
111300       IF KATH-VADIS-IDCHASSI-STA  NOT = ZERO                             
111400       OR KATH-VADIS-IDCHASSI-STO  NOT = ZERO                             
111500         IF KATH-VADIS-IDMODELL  = SPACE                                  
111600           MOVE NEJ TO KOMBINATION-SW                                     
111700           MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDMODELL-ATTR (INDX)           
111800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCHASSI-STA-ATTR (INDX)         
111900                                     MOD-IDCHASSI-STO-ATTR (INDX)         
112000         END-IF                                                           
112100         IF KATH-VADIS-KDCHATYP  = ZERO                                   
112200           MOVE NEJ TO KOMBINATION-SW                                     
112300           MOVE MFS-NUM-FAELT-FEL  TO MOD-KDCHATYP-ATTR (INDX)            
112400           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCHASSI-STA-ATTR (INDX)         
112500                                     MOD-IDCHASSI-STO-ATTR (INDX)         
112600         END-IF                                                           
112700       END-IF                                                             
112800                                                                          
112900       IF KATH-VADIS-TIMODAAR-STA = ZERO                                  
113000       AND KATH-VADIS-IDCHASSI-STA NOT = ZERO                             
113100         MOVE NEJ TO KOMBINATION-SW                                       
113200         MOVE MFS-NUM-FAELT-FEL TO MOD-TIMODAAR-STA-ATTR (INDX)           
113300                                   MOD-IDCHASSI-STA-ATTR (INDX)           
113400       END-IF                                                             
113500                                                                          
113600       IF KATH-VADIS-TIMODAAR-STO = ZERO                                  
113700       AND KATH-VADIS-IDCHASSI-STO NOT = ZERO                             
113800         MOVE NEJ TO KOMBINATION-SW                                       
113900         MOVE MFS-NUM-FAELT-FEL TO MOD-TIMODAAR-STO-ATTR (INDX)           
114000                                   MOD-IDCHASSI-STO-ATTR (INDX)           
114100       END-IF                                                             
114200                                                                          
114300       IF KATH-VADIS-FLEXCL  = JA                                         
114400       AND KATH-VADIS-IDVARIANT-2 NOT = SPACE                             
114500         MOVE NEJ TO KOMBINATION-SW                                       
114600         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDVARIANT-ATTR (INDX)             
114700                                    MOD-IDVARIANT-2-ATTR (INDX)           
114800       END-IF                                                             
114900                                                                          
115000       IF KATH-VADIS-IDCHASSI-STA  NOT = ZERO                             
115100       AND KATH-VADIS-IDCHASSI-STO  NOT = ZERO                            
115200         IF KATH-VADIS-IDCHASSI-STA > KATH-VADIS-IDCHASSI-STO             
115300           MOVE NEJ TO INTERVALL-SW                                       
115400           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCHASSI-STA-ATTR (INDX)         
115500                                     MOD-IDCHASSI-STO-ATTR (INDX)         
115600         END-IF                                                           
115700       END-IF                                                             
115800                                                                          
115900       IF KATH-VADIS-IDCHASSI-STA  NOT = ZERO                             
116000       AND KATH-VADIS-TIMODAAR-STA  = ZERO                                
116100         MOVE NEJ TO KOMBINATION-SW                                       
116200         MOVE MFS-NUM-FAELT-FEL TO MOD-TIMODAAR-STA-ATTR (INDX)           
116300                                   MOD-IDCHASSI-STA-ATTR (INDX)           
116400       END-IF                                                             
116500       IF KATH-VADIS-IDCHASSI-STO  NOT = ZERO                             
116600       AND KATH-VADIS-TIMODAAR-STO  = ZERO                                
116700         MOVE NEJ TO KOMBINATION-SW                                       
116800         MOVE MFS-NUM-FAELT-FEL TO MOD-TIMODAAR-STO-ATTR (INDX)           
116900                                   MOD-IDCHASSI-STO-ATTR (INDX)           
117000       END-IF                                                             
117100                                                                          
117200     END-IF                                                               
117300                                                                          
117400     IF KOMBINATION-FEL                                                   
117500       MOVE ERR-INVALID-COMBINATION TO MED-IDMFSFEL                       
117600       MOVE NEJ TO INDATA-SW                                              
117700     ELSE                                                                 
117800       IF INTERVALL-FEL                                                   
117900         MOVE ERR-FOM-STOERRE-TOM     TO MED-IDMFSFEL                     
118000         MOVE NEJ TO INDATA-SW                                            
118100       END-IF                                                             
118200     END-IF                                                               
118300     .                                                                    
118400     EJECT                                                                
118500 GD-KOLLA-RADINPUT-MOT-WDR2 SECTION.                                      
118600                                                                          
118700*    -- SPAR DLI-IO-AREAN INNAN NYA BASER LÄSES                           
118800     MOVE KATH-VADIS-WDN513 TO SPAR-VADIS-WDN513                          
118900*    -- MARKERA FELMEDDELANDE EJ ÄNNU SATT                                
119000     MOVE ZERO TO MED-IDMFSFEL                                            
119100                                                                          
119200     IF SPAR-VADIS-IDMODELL NOT = SPACE                                   
119300                                                                          
119400       MOVE SPAR-VADIS-IDMODELL TO W-IDMODELL-1212                        
119500       PERFORM IMS-GET-WDGX1212                                           
119600       IF SEGMENT-SAKNAS                                                  
119700         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDMODELL-ATTR (INDX)            
119800         MOVE NEJ TO INDATA-SW                                            
119900       ELSE                                                               
120000         IF 1212-TIMODAAR-STA = ZERO                                      
120100           MOVE 1974 TO 1212-TIMODAAR-STA                                 
120200         END-IF                                                           
120300         IF 1212-TIMODAAR-STO = ZERO                                      
120400           MOVE 2-AAR-FRAM TO 1212-TIMODAAR-STO                           
120500         END-IF                                                           
120600         IF SPAR-VADIS-TIMODAAR-STA NOT = ZERO                            
120700         AND (SPAR-VADIS-TIMODAAR-STA < 1212-TIMODAAR-STA                 
120800                                  OR  > 1212-TIMODAAR-STO)                
120900             MOVE MFS-NUM-FAELT-FEL TO                                    
121000                                    MOD-TIMODAAR-STA-ATTR (INDX)          
121100             MOVE MFS-ALFA-FAELT-FEL TO                                   
121200                                    MOD-IDMODELL-ATTR (INDX)              
121300             MOVE NEJ TO INDATA-SW                                        
121400         END-IF                                                           
121500         IF SPAR-VADIS-TIMODAAR-STO NOT = ZERO                            
121600         AND (SPAR-VADIS-TIMODAAR-STO < 1212-TIMODAAR-STA                 
121700                                  OR  > 1212-TIMODAAR-STO)                
121800             MOVE MFS-NUM-FAELT-FEL TO                                    
121900                                    MOD-TIMODAAR-STO-ATTR (INDX)          
122000             MOVE MFS-ALFA-FAELT-FEL TO                                   
122100                                    MOD-IDMODELL-ATTR (INDX)              
122200             MOVE NEJ TO INDATA-SW                                        
122300         END-IF                                                           
122400       END-IF                                                             
122500                                                                          
122600     END-IF                                                               
122700                                                                          
122800                                                                          
122900     IF SPAR-VADIS-IDVARIANT NOT = SPACE                                  
123000                                                                          
123100       MOVE SPAR-VADIS-IDVARIANT TO W-IDVARIANT-1214                      
123200       PERFORM IMS-GET-WDGX1214                                           
123300       IF SEGMENT-SAKNAS                                                  
123400         MOVE ZERO TO SPAR-1214-IDKONFIG                                  
123500         MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDVARIANT-ATTR (INDX)            
123600         MOVE NEJ TO INDATA-SW                                            
123700       ELSE                                                               
123800         MOVE 1214-IDKONFIG TO SPAR-1214-IDKONFIG                         
123900         IF 1214-TIVARAAR-STA = ZERO                                      
124000           MOVE 1974 TO 1214-TIVARAAR-STA                                 
124100         END-IF                                                           
124200         IF 1214-TIVARAAR-STO = ZERO                                      
124300           MOVE 2-AAR-FRAM TO 1214-TIVARAAR-STO                           
124400         END-IF                                                           
124500         IF SPAR-VADIS-TIMODAAR-STA NOT = ZERO                            
124600         AND (SPAR-VADIS-TIMODAAR-STA < 1214-TIVARAAR-STA                 
124700                                  OR  > 1214-TIVARAAR-STO)                
124800           MOVE MFS-NUM-FAELT-FEL TO                                      
124900                                  MOD-TIMODAAR-STA-ATTR (INDX)            
125000           MOVE MFS-ALFA-FAELT-FEL TO                                     
125100                                  MOD-IDVARIANT-ATTR (INDX)               
125200           MOVE NEJ TO INDATA-SW                                          
125300         END-IF                                                           
125400         IF SPAR-VADIS-TIMODAAR-STO NOT = ZERO                            
125500         AND (SPAR-VADIS-TIMODAAR-STO < 1214-TIVARAAR-STA                 
125600                                  OR  > 1214-TIVARAAR-STO)                
125700             MOVE MFS-NUM-FAELT-FEL TO                                    
125800                                    MOD-TIMODAAR-STO-ATTR (INDX)          
125900             MOVE MFS-ALFA-FAELT-FEL TO                                   
126000                                    MOD-IDVARIANT-ATTR (INDX)             
126100             MOVE NEJ TO INDATA-SW                                        
126200         END-IF                                                           
126300         IF SPAR-VADIS-IDMODELL NOT = SPACE                               
126400           MOVE SPAR-VADIS-IDMODELL    TO W-IDMODELL-1215                 
126500           MOVE SPAR-VADIS-IDVARIANT TO W-IDVARIANT-1216                  
126600           PERFORM IMS-GET-WDGX1216                                       
126700           IF SEGMENT-SAKNAS                                              
126800             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMODELL-ATTR (INDX)          
126900                                     MOD-IDVARIANT-ATTR (INDX)            
127000             MOVE NEJ TO INDATA-SW                                        
127100           ELSE                                                           
127200             IF 1216-TIMOVAAR-STA = ZERO                                  
127300               MOVE 1974 TO 1216-TIMOVAAR-STA                             
127400             END-IF                                                       
127500             IF 1216-TIMOVAAR-STO = ZERO                                  
127600               MOVE 2-AAR-FRAM TO 1216-TIMOVAAR-STO                       
127700             END-IF                                                       
127800             IF SPAR-VADIS-TIMODAAR-STA NOT = ZERO                        
127900             AND (SPAR-VADIS-TIMODAAR-STA < 1216-TIMOVAAR-STA             
128000                                      OR  > 1216-TIMOVAAR-STO)            
128100               MOVE MFS-NUM-FAELT-FEL  TO                                 
128200                                      MOD-TIMODAAR-STA-ATTR (INDX)        
128300               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMODELL-ATTR (INDX)        
128400                                      MOD-IDVARIANT-ATTR (INDX)           
128500               MOVE NEJ TO INDATA-SW                                      
128600             END-IF                                                       
128700             IF SPAR-VADIS-TIMODAAR-STO NOT = ZERO                        
128800             AND (SPAR-VADIS-TIMODAAR-STO < 1216-TIMOVAAR-STA             
128900                                      OR  > 1216-TIMOVAAR-STO)            
129000               MOVE MFS-NUM-FAELT-FEL TO                                  
129100                                      MOD-TIMODAAR-STO-ATTR (INDX)        
129200               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMODELL-ATTR (INDX)        
129300                                      MOD-IDVARIANT-ATTR (INDX)           
129400               MOVE NEJ TO INDATA-SW                                      
129500             END-IF                                                       
129600           END-IF                                                         
129700         END-IF                                                           
129800       END-IF                                                             
129900                                                                          
130000     END-IF                                                               
130100                                                                          
130200                                                                          
130300     IF SPAR-VADIS-IDVARIANT-2 NOT = SPACE                                
130400                                                                          
130500       MOVE SPAR-VADIS-IDVARIANT-2 TO W-IDVARIANT-1214                    
130600       PERFORM IMS-GET-WDGX1214                                           
130700       IF SEGMENT-SAKNAS                                                  
130800*        -- FLYTTA KONSTIGT VÄRDE TILL IDKONFIG FÖR ATT                   
130900*        -- LÄNGRE NED EJ SKA GE LIKHET.                                  
131000         MOVE 9 TO 1214-IDKONFIG                                          
131100         MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDVARIANT-2-ATTR (INDX)          
131200         MOVE NEJ TO INDATA-SW                                            
131300       ELSE                                                               
131400         IF 1214-TIVARAAR-STA = ZERO                                      
131500           MOVE 1974 TO 1214-TIVARAAR-STA                                 
131600         END-IF                                                           
131700         IF 1214-TIVARAAR-STO = ZERO                                      
131800           MOVE 2-AAR-FRAM TO 1214-TIVARAAR-STO                           
131900         END-IF                                                           
132000         IF SPAR-VADIS-TIMODAAR-STA NOT = ZERO                            
132100         AND (SPAR-VADIS-TIMODAAR-STA < 1214-TIVARAAR-STA                 
132200                                  OR  > 1214-TIVARAAR-STO)                
132300           MOVE MFS-NUM-FAELT-FEL TO                                      
132400                                  MOD-TIMODAAR-STA-ATTR (INDX)            
132500           MOVE MFS-ALFA-FAELT-FEL TO                                     
132600                                  MOD-IDVARIANT-2-ATTR (INDX)             
132700           MOVE NEJ TO INDATA-SW                                          
132800         END-IF                                                           
132900         IF SPAR-VADIS-TIMODAAR-STO NOT = ZERO                            
133000         AND (SPAR-VADIS-TIMODAAR-STO < 1214-TIVARAAR-STA                 
133100                                  OR  > 1214-TIVARAAR-STO)                
133200             MOVE MFS-NUM-FAELT-FEL TO                                    
133300                                    MOD-TIMODAAR-STO-ATTR (INDX)          
133400             MOVE MFS-ALFA-FAELT-FEL TO                                   
133500                                    MOD-IDVARIANT-2-ATTR (INDX)           
133600             MOVE NEJ TO INDATA-SW                                        
133700         END-IF                                                           
133800         IF SPAR-VADIS-IDMODELL NOT = SPACE                               
133900           MOVE SPAR-VADIS-IDMODELL    TO W-IDMODELL-1215                 
134000           MOVE SPAR-VADIS-IDVARIANT-2 TO W-IDVARIANT-1216                
134100           PERFORM IMS-GET-WDGX1216                                       
134200           IF SEGMENT-SAKNAS                                              
134300             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMODELL-ATTR (INDX)          
134400                                     MOD-IDVARIANT-2-ATTR (INDX)          
134500             MOVE NEJ TO INDATA-SW                                        
134600           ELSE                                                           
134700             IF 1216-TIMOVAAR-STA = ZERO                                  
134800               MOVE 1974 TO 1216-TIMOVAAR-STA                             
134900             END-IF                                                       
135000             IF 1216-TIMOVAAR-STO = ZERO                                  
135100               MOVE 2-AAR-FRAM TO 1216-TIMOVAAR-STO                       
135200             END-IF                                                       
135300             IF SPAR-VADIS-TIMODAAR-STA NOT = ZERO                        
135400             AND (SPAR-VADIS-TIMODAAR-STA < 1216-TIMOVAAR-STA             
135500                                      OR  > 1216-TIMOVAAR-STO)            
135600               MOVE MFS-NUM-FAELT-FEL  TO                                 
135700                                      MOD-TIMODAAR-STA-ATTR (INDX)        
135800               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMODELL-ATTR (INDX)        
135900                                      MOD-IDVARIANT-ATTR (INDX)           
136000               MOVE NEJ TO INDATA-SW                                      
136100             END-IF                                                       
136200             IF SPAR-VADIS-TIMODAAR-STO NOT = ZERO                        
136300             AND (SPAR-VADIS-TIMODAAR-STO < 1216-TIMOVAAR-STA             
136400                                      OR  > 1216-TIMOVAAR-STO)            
136500               MOVE MFS-NUM-FAELT-FEL TO                                  
136600                                      MOD-TIMODAAR-STO-ATTR (INDX)        
136700               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMODELL-ATTR (INDX)        
136800                                      MOD-IDVARIANT-ATTR (INDX)           
136900               MOVE NEJ TO INDATA-SW                                      
137000             END-IF                                                       
137100           END-IF                                                         
137200         END-IF                                                           
137300       END-IF                                                             
137400                                                                          
137500     END-IF                                                               
137600                                                                          
137700     IF SPAR-VADIS-IDVARIANT NOT = SPACE                                  
137800     AND SPAR-VADIS-IDVARIANT-2 NOT = SPACE                               
137900       IF SPAR-1214-IDKONFIG  = 1214-IDKONFIG                             
138000         MOVE NEJ TO INDATA-SW                                            
138100         MOVE ERR-SAMMA-KONFIGTYP TO MED-IDMFSFEL                         
138200         MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDVARIANT-ATTR (INDX)            
138300                                     MOD-IDVARIANT-2-ATTR (INDX)          
138400       END-IF                                                             
138500     END-IF                                                               
138600                                                                          
138700     IF SPAR-VADIS-IDCHASSI-STA NOT = ZERO                                
138800     OR SPAR-VADIS-IDCHASSI-STO NOT = ZERO                                
138900                                                                          
139000       MOVE SPAR-VADIS-IDMODELL TO W-IDMODELL-1218                        
139100       MOVE SPAR-VADIS-KDCHATYP TO W-KDCHATYP-1218                        
139200                                                                          
139300       IF SPAR-VADIS-TIMODAAR-STA NOT = ZERO                              
139400         MOVE SPAR-VADIS-TIMODAAR-STA TO W-TICHAAAR-1218                  
139500         PERFORM IMS-GET-WDGX1218                                         
139600         IF SEGMENT-SAKNAS                                                
139700           MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCHATYP-ATTR (INDX)           
139800           MOVE MFS-NUM-FAELT-FEL   TO                                    
139900                                   MOD-TIMODAAR-STA-ATTR (INDX)           
140000           MOVE NEJ TO INDATA-SW                                          
140100         ELSE                                                             
140200           IF 1218-IDCHASSI-STA = ZERO                                    
140300             MOVE 1 TO 1218-IDCHASSI-STA                                  
140400           END-IF                                                         
140500           IF 1218-IDCHASSI-STO = ZERO                                    
140600             MOVE 999999  TO 1218-IDCHASSI-STO                            
140700           END-IF                                                         
140800           IF SPAR-VADIS-IDCHASSI-STA < 1218-IDCHASSI-STA                 
140900           OR                         > 1218-IDCHASSI-STO                 
141000             MOVE MFS-NUM-FAELT-FEL   TO MOD-KDCHATYP-ATTR (INDX)         
141100                                     MOD-IDCHASSI-STA-ATTR (INDX)         
141200             MOVE NEJ TO INDATA-SW                                        
141300           END-IF                                                         
141400         END-IF                                                           
141500       END-IF                                                             
141600       IF SPAR-VADIS-TIMODAAR-STO NOT = ZERO                              
141700         MOVE SPAR-VADIS-TIMODAAR-STO TO W-TICHAAAR-1218                  
141800         PERFORM IMS-GET-WDGX1218                                         
141900         IF SEGMENT-SAKNAS                                                
142000           MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCHATYP-ATTR (INDX)           
142100           MOVE MFS-NUM-FAELT-FEL   TO                                    
142200                                   MOD-TIMODAAR-STO-ATTR (INDX)           
142300           MOVE NEJ TO INDATA-SW                                          
142400         ELSE                                                             
142500           IF 1218-IDCHASSI-STA = ZERO                                    
142600             MOVE 1 TO 1218-IDCHASSI-STA                                  
142700           END-IF                                                         
142800           IF 1218-IDCHASSI-STO = ZERO                                    
142900             MOVE 999999  TO 1218-IDCHASSI-STO                            
143000           END-IF                                                         
143100           IF SPAR-VADIS-IDCHASSI-STO NOT = ZERO AND                      
143200             (SPAR-VADIS-IDCHASSI-STO < 1218-IDCHASSI-STA                 
143300              OR                      > 1218-IDCHASSI-STO)                
143400             MOVE MFS-NUM-FAELT-FEL   TO MOD-KDCHATYP-ATTR (INDX)         
143500             MOVE MFS-NUM-FAELT-FEL   TO                                  
143600                                     MOD-IDCHASSI-STO-ATTR (INDX)         
143700             MOVE NEJ TO INDATA-SW                                        
143800           END-IF                                                         
143900         END-IF                                                           
144000       END-IF                                                             
144100                                                                          
144200     END-IF                                                               
144300                                                                          
144400     IF INDATA-FEL AND MED-IDMFSFEL = ZERO                                
144500       MOVE ERR-SAKNAS-I-BAS TO MED-IDMFSFEL                              
144600     END-IF                                                               
144700                                                                          
144800     .                                                                    
144900     EJECT                                                                
145000 H-UPPDATERA SECTION.                                                     
145100                                                                          
145200     PERFORM IMS-GET-KATH-AVS                                             
145300     IF SEGMENT-FINNS                                                     
145400                                                                          
145500*      -- SLÅ PÅ FLAGGA ATT AVSNITTET ÄNDRATS                             
145600*      -- SEN SENASTE VADIS-FÖDNINGEN                                     
145700       MOVE JA TO KATH-AVS-FLAVSUST                                       
145800                                                                          
145900       IF MID-FLAVSTVAD NOT = ALL '+'                                     
146000         MOVE MID-FLAVSTVAD TO KATH-AVS-FLAVSTVAD MOD-FLAVSTVAD           
146100         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLAVSTVAD-ATTR                 
146200                                                                          
146300         IF MID-FLAVSTVAD = JA                                            
146400           MOVE FRAGA-UPPD-AVS TO MED-IDMFSINF                            
146500           CALL WMEDKONV USING MED-WMEDAREA                               
146600           MOVE MED-TEMFSINF TO MOD-TEMFSFEL                              
146700         END-IF                                                           
146800       ELSE                                                               
146900         MOVE MFS-ROER-EJ-FAELT TO MOD-FLAVSTVAD                          
147000       END-IF                                                             
147100                                                                          
147200       PERFORM IMS-REPL-KATH                                              
147300                                                                          
147400       MOVE 1 TO INDX                                                     
147500       PERFORM UNTIL INDX > MAX-INDX                                      
147600         PERFORM S11-KOLLA-OM-RADINPUT                                    
147700         IF INPUT-FINNS                                                   
147800           PERFORM HA-UPPDATERA-RADDATA                                   
147900         END-IF                                                           
148000         ADD 1 TO INDX                                                    
148100       END-PERFORM                                                        
148200                                                                          
148300       IF RADNR-OVERFLOW                                                  
148400*        -- RADNUMMER-SERIEN HAR SLAGIT RUNT.                             
148500*        -- INGEN ISRT HAR GJORTS I HA-SEKTIONEN                          
148600         MOVE ERR-FEL-RADNR TO MED-IDMFSINF                               
148700       ELSE                                                               
148800         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
148900       END-IF                                                             
149000       CALL WMEDKONV USING MED-WMEDAREA                                   
149100       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
149200       PERFORM MFS-FORM-ATTR                                              
149300       PERFORM MFS-RENSA-FAELT-IN                                         
149400                                                                          
149500     ELSE                                                                 
149600*      - INGET AVSNITTSSEGMENT FINNS!                                     
149700*        FELMEDDELANDE SÄTTS I F-SEKTIONEN                                
149800       CONTINUE                                                           
149900     END-IF                                                               
150000     .                                                                    
150100     EJECT                                                                
150200 HA-UPPDATERA-RADDATA SECTION.                                            
150300                                                                          
150400     IF MID-IDRADNR(INDX) NUMERIC   AND MID-IDRADNR(INDX) > ZERO          
150500*      -- GAMMAL RAD. ANVÄND BEFINTLIGT RADNR.                            
150600       MOVE MID-IDRADNR(INDX) TO W-IDRADN                                 
150700     ELSE                                                                 
150800*      -- NYTILLAGD RAD. ÖKA SENAST ANVÄNDA RADNR ELLER                   
150900*      -- TA REDA PÅ LÄMPLIGT VÄRDE OM FÖRSTA GÅNGEN.                     
151000       IF WS-IDRADN-SLU = ZERO                                            
151100         PERFORM IMS-GETL-KATH-VADIS                                      
151200         IF SEGMENT-FINNS                                                 
151300           MOVE KATH-VADIS-IDRADN TO WS-IDRADN-SLU                        
151400         END-IF                                                           
151500       END-IF                                                             
151600       ADD 1 TO WS-IDRADN-SLU                                             
151700       MOVE WS-IDRADN-SLU TO W-IDRADN                                     
151800     END-IF                                                               
151900                                                                          
152000     PERFORM IMS-GETU-KATH-VADIS                                          
152100     IF SEGMENT-SAKNAS                                                    
152200       PERFORM S12-INIT-KATH-VADIS                                        
152300     END-IF                                                               
152400     PERFORM S13-MID-TILL-KATH-VADIS                                      
152500                                                                          
152600     IF  KATH-VADIS-IDKOL        = SPACE                                  
152700     AND KATH-VADIS-FLEXCL       = NEJ                                    
152800     AND KATH-VADIS-IDMODELL     = SPACE                                  
152900     AND KATH-VADIS-TIMODAAR-STA = ZERO                                   
153000     AND KATH-VADIS-TIMODAAR-STO = ZERO                                   
153100     AND KATH-VADIS-IDVARIANT    = SPACE                                  
153200     AND KATH-VADIS-IDVARIANT-2  = SPACE                                  
153300     AND KATH-VADIS-KDCHATYP     = ZERO                                   
153400     AND KATH-VADIS-IDCHASSI-STA = ZERO                                   
153500     AND KATH-VADIS-IDCHASSI-STO = ZERO                                   
153600       IF SEGMENT-FINNS                                                   
153700         PERFORM IMS-DLET-KATH                                            
153800       ELSE                                                               
153900         CONTINUE                                                         
154000       END-IF                                                             
154100     ELSE                                                                 
154200       IF SEGMENT-FINNS                                                   
154300         PERFORM IMS-REPL-KATH                                            
154400       ELSE                                                               
154500         IF KATH-VADIS-IDRADN NOT = ZERO                                  
154600           PERFORM IMS-ISRT-KATH-VADIS                                    
154700         ELSE                                                             
154800*        -- RADNUMREN HAR SLAGIT RUNT FRÅN 9999 -> 0000                   
154900*        -- NYA RADER KAN EJ LÄNGRE LÄGGAS TILL PÅ AVSNITTET              
155000*        -- UTAN MAN MÅSTE TA BORT ALLA OCH LÄGGA UPP PÅ NYTT.            
155100*        -- FELMEDDELANDE SÄTTS I H-SEKTIONEN.                            
155200           MOVE NEJ TO RADNR-SW                                           
155300         END-IF                                                           
155400       END-IF                                                             
155500     END-IF                                                               
155600     .                                                                    
155700     EJECT                                                                
155800 I-KOLLA-ATT-INGET-INMATAT SECTION.                                       
155900                                                                          
156000     PERFORM S10-KOLLA-OM-INPUT                                           
156100     IF INPUT-FINNS                                                       
156200       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
156300       CALL WMEDKONV USING MED-WMEDAREA                                   
156400       MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                  
156500       MOVE NEJ TO INDATA-SW                                              
156600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
156700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
156800       PERFORM MFS-LAESIN-MODIF-DATA-IGEN                                 
156900     END-IF                                                               
157000     .                                                                    
157100     EJECT                                                                
157200 K-BESTAEM-RAETT-PUBKOD SECTION.                                          
157300*    -- Kolla vilken pubkod som man skall läsa med                        
157400     PERFORM IMS-GET-KATH-AVS                                             
157500     IF SEGMENT-FINNS                                                     
157600*      -- Pröva först med inmatad pubkod = W-KDCATPUB-FOM                 
157700       MOVE '-' TO COMMAND-CODE                                           
157800       MOVE NEJ TO WS-RAETT-PUBKOD                                        
157900       PERFORM IMS-GNP-KATH-RAD-1                                         
158000       IF SEGMENT-FINNS                                                   
158100         PERFORM UNTIL SEGMENT-SAKNAS                                     
158200*          -- Hämtar matchande pubkod från rad 0001                       
158300           IF  W-KDCATPUB-FOM >= KATH-RAD-KDCATPUB-FOM                    
158400           AND W-KDCATPUB-FOM <= KATH-RAD-KDCATPUB-TOM                    
158500             MOVE KATH-RAD-KDCATPUB-FOM TO W-KDCATPUB-FOM                 
158600             MOVE KATH-RAD-KDCATPUB-TOM TO WS-KDCATPUB-TOM                
158700             MOVE JA TO WS-RAETT-PUBKOD                                   
158800           END-IF                                                         
158900           PERFORM IMS-GNP-KATH-RAD-1                                     
159000         END-PERFORM                                                      
159100                                                                          
159200         IF WS-RAETT-PUBKOD = NEJ                                         
159300*          -- Inmatad pubkod ligger antagligen före den första            
159400*          -- Hämta då den första förekomsten av rad 0001                 
159500           MOVE 'F' TO COMMAND-CODE                                       
159600           PERFORM IMS-GNP-KATH-RAD-1                                     
159700           IF SEGMENT-FINNS                                               
159800             MOVE KATH-RAD-KDCATPUB-FOM TO W-KDCATPUB-FOM                 
159900             MOVE KATH-RAD-KDCATPUB-TOM TO WS-KDCATPUB-TOM                
160000           END-IF                                                         
160100         END-IF                                                           
160200       END-IF                                                             
160300                                                                          
160400       MOVE W-KDCATPUB-FOM(4:3)  TO MOD-KDCATPUB-R-FOM-UT                 
160500       MOVE WS-KDCATPUB-TOM(4:3) TO MOD-KDCATPUB-R-TOM                    
160600       INSPECT MOD-KDCATPUB-R-TOM REPLACING ALL HIGH-VALUE BY '9'         
160700     END-IF                                                               
160800     .                                                                    
160900     EJECT                                                                
161000 S10-KOLLA-OM-INPUT SECTION.                                              
161100                                                                          
161200     MOVE NEJ TO INPUT-FINNS-SW                                           
161300     IF MID-FLAVSTVAD NOT = ALL '+'                                       
161400       MOVE JA TO INPUT-FINNS-SW                                          
161500     END-IF                                                               
161600                                                                          
161700     MOVE +1 TO INDX                                                      
161800     PERFORM UNTIL INDX > MAX-INDX OR INPUT-FINNS                         
161900       PERFORM S11-KOLLA-OM-RADINPUT                                      
162000       ADD +1 TO INDX                                                     
162100     END-PERFORM                                                          
162200     .                                                                    
162300     SKIP3                                                                
162400 S11-KOLLA-OM-RADINPUT SECTION.                                           
162500                                                                          
162600     MOVE NEJ TO INPUT-FINNS-SW                                           
162700     IF  MID-IDKOL (INDX)          NOT = ALL '+'                          
162800     OR  MID-FLEXCL (INDX)         NOT = ALL '+'                          
162900     OR  MID-IDMODELL (INDX)       NOT = ALL '+'                          
163000     OR  MID-TIMODAAR-STA (INDX)   NOT = ALL '+'                          
163100     OR  MID-TIMODAAR-STO (INDX)   NOT = ALL '+'                          
163200     OR  MID-IDVARIANT (INDX)      NOT = ALL '+'                          
163300     OR  MID-IDVARIANT-2 (INDX)    NOT = ALL '+'                          
163400     OR  MID-KDCHATYP (INDX)       NOT = ALL '+'                          
163500     OR  MID-IDCHASSI-STA (INDX)   NOT = ALL '+'                          
163600     OR  MID-IDCHASSI-STO (INDX)   NOT = ALL '+'                          
163700       MOVE JA TO INPUT-FINNS-SW                                          
163800     END-IF                                                               
163900     .                                                                    
164000     EJECT                                                                
164100 S12-INIT-KATH-VADIS  SECTION.                                            
164200                                                                          
164300     MOVE W-IDRADN       TO KATH-VADIS-IDRADN                             
164400     MOVE W-KDCATPUB-FOM TO KATH-VADIS-KDCATPUB-FOM                       
164500     MOVE WS-KDCATPUB-TOM TO KATH-VADIS-KDCATPUB-TOM                      
164600                                                                          
164700     MOVE SPACE     TO KATH-VADIS-IDKOL                                   
164800     MOVE NEJ       TO KATH-VADIS-FLEXCL                                  
164900     MOVE SPACE     TO KATH-VADIS-IDMODELL                                
165000     MOVE ZERO      TO KATH-VADIS-TIMODAAR-STA                            
165100     MOVE ZERO      TO KATH-VADIS-TIMODAAR-STO                            
165200     MOVE SPACE     TO KATH-VADIS-IDVARIANT                               
165300     MOVE SPACE     TO KATH-VADIS-IDVARIANT-2                             
165400     MOVE ZERO      TO KATH-VADIS-KDCHATYP                                
165500     MOVE ZERO      TO KATH-VADIS-IDCHASSI-STA                            
165600     MOVE ZERO      TO KATH-VADIS-IDCHASSI-STO                            
165700                                                                          
165800     EJECT                                                                
165900     .                                                                    
166000 S13-MID-TILL-KATH-VADIS  SECTION.                                        
166100                                                                          
166200     IF MID-IDKOL (INDX) NOT = ALL '+'                                    
166300       MOVE MID-IDKOL (INDX) TO KATH-VADIS-IDKOL                          
166400     END-IF                                                               
166500                                                                          
166600     IF MID-FLEXCL (INDX) NOT = ALL '+'                                   
166700       IF MID-FLEXCL (INDX) =  NEJ                                        
166800         MOVE NEJ   TO KATH-VADIS-FLEXCL                                  
166900         MOVE SPACE TO MOD-FLEXCL (INDX)                                  
167000       END-IF                                                             
167100       IF MID-FLEXCL (INDX) =  JA                                         
167200         MOVE JA    TO KATH-VADIS-FLEXCL                                  
167300         MOVE 'X'   TO MOD-FLEXCL (INDX)                                  
167400       END-IF                                                             
167500     END-IF                                                               
167600                                                                          
167700     IF MID-IDMODELL (INDX) NOT = ALL '+'                                 
167800       MOVE MID-IDMODELL (INDX) TO KATH-VADIS-IDMODELL                    
167900     END-IF                                                               
168000                                                                          
168100     IF MID-TIMODAAR-STA (INDX) NOT = ALL '+'                             
168200       MOVE MID-TIMODAAR-STA (INDX) TO KATH-VADIS-TIMODAAR-STA            
168300     END-IF                                                               
168400                                                                          
168500     IF MID-TIMODAAR-STO (INDX) NOT = ALL '+'                             
168600       MOVE MID-TIMODAAR-STO (INDX) TO KATH-VADIS-TIMODAAR-STO            
168700     END-IF                                                               
168800                                                                          
168900     IF MID-IDVARIANT (INDX) NOT = ALL '+'                                
169000       MOVE MID-IDVARIANT (INDX) TO KATH-VADIS-IDVARIANT                  
169100     END-IF                                                               
169200                                                                          
169300     IF MID-IDVARIANT-2 (INDX) NOT = ALL '+'                              
169400       MOVE MID-IDVARIANT-2 (INDX) TO KATH-VADIS-IDVARIANT-2              
169500     END-IF                                                               
169600                                                                          
169700     IF MID-KDCHATYP (INDX) NOT = ALL '+'                                 
169800       MOVE MID-KDCHATYP (INDX) TO KATH-VADIS-KDCHATYP                    
169900     END-IF                                                               
170000                                                                          
170100     IF MID-IDCHASSI-STA (INDX) NOT = ALL '+'                             
170200       MOVE MID-IDCHASSI-STA (INDX) TO KATH-VADIS-IDCHASSI-STA            
170300     END-IF                                                               
170400                                                                          
170500     IF MID-IDCHASSI-STO (INDX) NOT = ALL '+'                             
170600       MOVE MID-IDCHASSI-STO (INDX) TO KATH-VADIS-IDCHASSI-STO            
170700     END-IF                                                               
170800     .                                                                    
170900     EJECT                                                                
171000 MFS-LAESIN-MODIF-DATA-IGEN   SECTION.                                    
171100                                                                          
171200     IF MID-FLAVSTVAD NOT = ALL '+'                                       
171300       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-FLAVSTVAD-ATTR                 
171400     END-IF                                                               
171500                                                                          
171600     MOVE +1 TO INDX                                                      
171700     PERFORM UNTIL INDX > MAX-INDX                                        
171800                                                                          
171900       IF  MID-IDKOL (INDX) NOT = ALL '+'                                 
172000         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDKOL-ATTR (INDX)             
172100       END-IF                                                             
172200                                                                          
172300       IF  MID-FLEXCL (INDX) NOT = ALL '+'                                
172400         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-FLEXCL-ATTR (INDX)            
172500       END-IF                                                             
172600                                                                          
172700       IF  MID-IDMODELL (INDX) NOT = ALL '+'                              
172800         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDMODELL-ATTR (INDX)          
172900       END-IF                                                             
173000                                                                          
173100       IF  MID-TIMODAAR-STA (INDX) NOT = ALL '+'                          
173200         MOVE MFS-ADD-LAES-IN-FAELT  TO                                   
173300                                    MOD-TIMODAAR-STA-ATTR (INDX)          
173400       END-IF                                                             
173500                                                                          
173600       IF  MID-TIMODAAR-STO (INDX) NOT = ALL '+'                          
173700         MOVE MFS-ADD-LAES-IN-FAELT  TO                                   
173800                                    MOD-TIMODAAR-STO-ATTR (INDX)          
173900       END-IF                                                             
174000                                                                          
174100       IF  MID-IDVARIANT (INDX) NOT = ALL '+'                             
174200         MOVE MFS-ADD-LAES-IN-FAELT  TO                                   
174300                                    MOD-IDVARIANT-ATTR (INDX)             
174400       END-IF                                                             
174500                                                                          
174600       IF  MID-IDVARIANT-2 (INDX) NOT = ALL '+'                           
174700         MOVE MFS-ADD-LAES-IN-FAELT  TO                                   
174800                                    MOD-IDVARIANT-2-ATTR (INDX)           
174900       END-IF                                                             
175000                                                                          
175100       IF  MID-KDCHATYP (INDX) NOT = ALL '+'                              
175200         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDCHATYP-ATTR (INDX)          
175300       END-IF                                                             
175400                                                                          
175500       IF  MID-IDCHASSI-STA (INDX) NOT = ALL '+'                          
175600         MOVE MFS-ADD-LAES-IN-FAELT  TO                                   
175700                                    MOD-IDCHASSI-STA-ATTR (INDX)          
175800       END-IF                                                             
175900                                                                          
176000       IF  MID-IDCHASSI-STO (INDX) NOT = ALL '+'                          
176100         MOVE MFS-ADD-LAES-IN-FAELT  TO                                   
176200                                    MOD-IDCHASSI-STO-ATTR (INDX)          
176300       END-IF                                                             
176400                                                                          
176500       ADD +1 TO INDX                                                     
176600     END-PERFORM                                                          
176700     .                                                                    
176800     EJECT                                                                
176900 MFS-RENSA-FAELT-UT SECTION.                                              
177000                                                                          
177100     MOVE MFS-RENSA-FAELT TO WS-MFSKOD                                    
177200     PERFORM MFS-BEHANDLA-FAELT-UT                                        
177300     .                                                                    
177400     SKIP3                                                                
177500 MFS-RENSA-RADDATA-UT SECTION.                                            
177600                                                                          
177700     MOVE MFS-RENSA-FAELT TO WS-MFSKOD                                    
177800     PERFORM MFS-BEHANDLA-RADDATA-UT                                      
177900     .                                                                    
178000     SKIP3                                                                
178100     SKIP3                                                                
178200 MFS-RENSA-FAELT-IN SECTION.                                              
178300                                                                          
178400     MOVE MFS-RENSA-FAELT TO WS-MFSKOD                                    
178500     PERFORM MFS-BEHANDLA-FAELT-IN                                        
178600     .                                                                    
178700     EJECT                                                                
178800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
178900                                                                          
179000     MOVE MFS-ROER-EJ-FAELT TO WS-MFSKOD                                  
179100     PERFORM MFS-BEHANDLA-FAELT-UT                                        
179200     .                                                                    
179300     SKIP3                                                                
179400 MFS-ROER-EJ-FAELT-IN SECTION.                                            
179500                                                                          
179600     MOVE MFS-ROER-EJ-FAELT TO WS-MFSKOD                                  
179700     PERFORM MFS-BEHANDLA-FAELT-IN                                        
179800     .                                                                    
179900     SKIP3                                                                
180000     SKIP3                                                                
180100 MFS-FORM-ATTR SECTION.                                                   
180200                                                                          
180300     MOVE MFS-FORMATETS-ATTR TO WS-MFSKOD                                 
180400     PERFORM MFS-BEHANDLA-ATTR                                            
180500     .                                                                    
180600     EJECT                                                                
180700 MFS-BEHANDLA-FAELT-UT SECTION.                                           
180800                                                                          
180900*    --- ALLA UTDATA-FÄLT                                                 
181000*    --- INKL. BLÄDDRINGSNYCKLAR                                          
181100     MOVE WS-MFSKOD       TO MOD-FLAVSTVAD                                
181200                                                                          
181300     MOVE 1 TO INDX                                                       
181400     PERFORM UNTIL INDX > MAX-INDX                                        
181500       PERFORM MFS-BEHANDLA-RADDATA-UT                                    
181600       ADD 1 TO INDX                                                      
181700     END-PERFORM                                                          
181800     .                                                                    
181900     SKIP3                                                                
182000 MFS-BEHANDLA-RADDATA-UT SECTION.                                         
182100                                                                          
182200     MOVE WS-MFSKOD     TO MOD-IDKOL        (INDX)                        
182300                           MOD-FLEXCL       (INDX)                        
182400                           MOD-IDMODELL     (INDX)                        
182500                           MOD-TIMODAAR-STA (INDX)                        
182600                           MOD-TIMODAAR-STO (INDX)                        
182700                           MOD-IDVARIANT    (INDX)                        
182800                           MOD-IDVARIANT-2  (INDX)                        
182900                           MOD-KDCHATYP     (INDX)                        
183000                           MOD-IDCHASSI-STA (INDX)                        
183100                           MOD-IDCHASSI-STO (INDX)                        
183200                           MOD-IDRADNR      (INDX)                        
183300     .                                                                    
183400     SKIP3                                                                
183500 MFS-BEHANDLA-FAELT-IN  SECTION.                                          
183600                                                                          
183700*    --- ALLA RENA INDATA-FÄLT                                            
183800     CONTINUE                                                             
183900     .                                                                    
184000     EJECT                                                                
184100 MFS-BEHANDLA-ATTR      SECTION.                                          
184200                                                                          
184300*    --- ALLA FÄLT MED ATTRIBUT                                           
184400     MOVE WS-MFSKOD       TO MOD-FLAVSTVAD-ATTR                           
184500                                                                          
184600     MOVE 1 TO INDX                                                       
184700     PERFORM UNTIL INDX > MAX-INDX                                        
184800       MOVE WS-MFSKOD     TO MOD-IDKOL-ATTR        (INDX)                 
184900                             MOD-FLEXCL-ATTR       (INDX)                 
185000                             MOD-IDMODELL-ATTR     (INDX)                 
185100                             MOD-TIMODAAR-STA-ATTR (INDX)                 
185200                             MOD-TIMODAAR-STO-ATTR (INDX)                 
185300                             MOD-IDVARIANT-ATTR    (INDX)                 
185400                             MOD-IDVARIANT-2-ATTR  (INDX)                 
185500                             MOD-KDCHATYP-ATTR     (INDX)                 
185600                             MOD-IDCHASSI-STA-ATTR (INDX)                 
185700                             MOD-IDCHASSI-STO-ATTR (INDX)                 
185800       ADD 1 TO INDX                                                      
185900     END-PERFORM                                                          
186000     .                                                                    
186100     EJECT                                                                
186200*                                                                         
186300* SECTION S50-Y2K-KDCATPUB-R LIGGER I                                     
186400* COPYTEXT W.PROD.COBOL.W150Y2K1                                          
186500*                                                                         
186600*    -COPY W150Y2K1                                                       
186700     EJECT                                                                
186800* --- IMS SEKTIONER ---                                                   
186900     SKIP3                                                                
187000 IMS-GET-MSG SECTION.                                                     
187100                                                                          
187200     MOVE '  QC' TO GODK-STATUSKODER                                      
187300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
187400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
187500     PERFORM IMS-STATUSKONTROLL                                           
187600     .                                                                    
187700     SKIP3                                                                
187800 IMS-INSERT-MSG SECTION.                                                  
187900                                                                          
188000     IF MSGI-IDLAND-SPR = 'SE'                                            
188100       MOVE '0' TO MFS-KDHUVOMR                                           
188200     END-IF                                                               
188300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
188400     MOVE SPACE TO GODK-STATUSKODER                                       
188500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
188600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
188700     PERFORM IMS-STATUSKONTROLL                                           
188800     .                                                                    
188900     EJECT                                                                
189000 IMS-GET-KATH-AVS SECTION.                                                
189100     SKIP2                                                                
189200     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
189300          DELIMITED BY SIZE INTO SSA1                                     
189400     MOVE '  GE' TO GODK-STATUSKODER                                      
189500     CALL CBLTDLI USING GHU KATH-PCB DLI-IO-AREA SSA1                     
189600     MOVE KATH-STATUS-CODE TO STATUS-WS                                   
189700     PERFORM IMS-STATUSKONTROLL                                           
189800     .                                                                    
189900     EJECT                                                                
190000 IMS-GNP-KATH-RAD-1 SECTION.                                              
190100     SKIP2                                                                
190200     STRING 'WLKATH12*' COMMAND-CODE                                      
190300                       '(IDCATRAD =0001'     ')'                          
190400          DELIMITED BY SIZE INTO SSA1                                     
190500     MOVE '  GE' TO GODK-STATUSKODER                                      
190600     CALL CBLTDLI USING GNP KATH-PCB DLI-IO-AREA SSA1                     
190700     MOVE KATH-STATUS-CODE TO STATUS-WS                                   
190800     PERFORM IMS-STATUSKONTROLL                                           
190900     .                                                                    
191000     EJECT                                                                
191100 IMS-GETU-KATH-VADIS SECTION.                                             
191200*    -- HÄMTA RAD MED ANGIVET NUMMER                                      
191300*    -- INOM DEN GIVNA PUBKODEN                                           
191400                                                                          
191500     STRING 'WLKATH01*P(WDN501KY =' W-WDN501KY-X ')'                      
191600          DELIMITED BY SIZE INTO SSA1                                     
191700     STRING 'WLKATH13(WDN513KY =' W-WDN513KY-X ')'                        
191800          DELIMITED BY SIZE INTO SSA2                                     
191900     MOVE '  GE' TO GODK-STATUSKODER                                      
192000     CALL CBLTDLI USING GHU KATH-PCB DLI-IO-AREA SSA1 SSA2                
192100     MOVE KATH-STATUS-CODE TO STATUS-WS                                   
192200     PERFORM IMS-STATUSKONTROLL                                           
192300     .                                                                    
192400     SKIP2                                                                
192500 IMS-GETF-KATH-VADIS SECTION.                                             
192600*    -- HÄMTA RAD FRÅN-OCH MED ANGIVET NUMMER                             
192700*    -- INOM DEN GIVNA PUBKODEN                                           
192800                                                                          
192900     STRING 'WLKATH13(IDRADNR >=' W-IDRADN-X                              
193000                 OCH 'KDCATPUF =' W-KDCATPUB-FOM ')'                      
193100          DELIMITED BY SIZE INTO SSA1                                     
193200     MOVE '  GE' TO GODK-STATUSKODER                                      
193300     CALL CBLTDLI USING GNP KATH-PCB DLI-IO-AREA SSA1                     
193400     MOVE KATH-STATUS-CODE TO STATUS-WS                                   
193500     PERFORM IMS-STATUSKONTROLL                                           
193600     .                                                                    
193700     SKIP2                                                                
193800 IMS-GETN-KATH-VADIS SECTION.                                             
193900*    -- HÄMTA NÄSTA RAD OAVSETT RADNR                                     
194000*    -- INOM DEN GIVNA PUBKODEN                                           
194100                                                                          
194200     STRING 'WLKATH13(KDCATPUF =' W-KDCATPUB-FOM ')'                      
194300          DELIMITED BY SIZE INTO SSA1                                     
194400     MOVE '  GE' TO GODK-STATUSKODER                                      
194500     CALL CBLTDLI USING GNP KATH-PCB DLI-IO-AREA SSA1                     
194600     MOVE KATH-STATUS-CODE TO STATUS-WS                                   
194700     PERFORM IMS-STATUSKONTROLL                                           
194800     .                                                                    
194900     EJECT                                                                
195000 IMS-GETL-KATH-VADIS SECTION.                                             
195100     SKIP2                                                                
195200*    -- HÄMTA SISTA EXISTERANDE RAD                                       
195300*    -- INOM DEN GIVNA PUBKODEN                                           
195400                                                                          
195500     STRING 'WLKATH01*P(WDN501KY =' W-WDN501KY-X ')'                      
195600          DELIMITED BY SIZE INTO SSA1                                     
195700     STRING 'WLKATH13*L(KDCATPUF =' W-KDCATPUB-FOM ')'                    
195800          DELIMITED BY SIZE INTO SSA2                                     
195900     MOVE '  GE' TO GODK-STATUSKODER                                      
196000     CALL CBLTDLI USING GU  KATH-PCB DLI-IO-AREA SSA1 SSA2                
196100     MOVE KATH-STATUS-CODE TO STATUS-WS                                   
196200     PERFORM IMS-STATUSKONTROLL                                           
196300     .                                                                    
196400     EJECT                                                                
196500 IMS-ISRT-KATH-VADIS SECTION.                                             
196600     SKIP2                                                                
196700     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
196800          DELIMITED BY SIZE INTO SSA1                                     
196900     MOVE 'WLKATH13 ' TO SSA2                                             
197000     MOVE '  '   TO GODK-STATUSKODER                                      
197100     CALL CBLTDLI USING ISRT KATH-PCB DLI-IO-AREA SSA1 SSA2               
197200     MOVE KATH-STATUS-CODE TO STATUS-WS                                   
197300     PERFORM IMS-STATUSKONTROLL                                           
197400     .                                                                    
197500     EJECT                                                                
197600 IMS-REPL-KATH SECTION.                                                   
197700     SKIP2                                                                
197800     MOVE '  ' TO GODK-STATUSKODER                                        
197900     CALL CBLTDLI USING REPL KATH-PCB DLI-IO-AREA                         
198000     MOVE KATH-STATUS-CODE TO STATUS-WS                                   
198100     PERFORM IMS-STATUSKONTROLL                                           
198200     .                                                                    
198300     SKIP3                                                                
198400                                                                          
198500                                                                          
198600 IMS-DLET-KATH SECTION.                                                   
198700     SKIP2                                                                
198800     MOVE '  ' TO GODK-STATUSKODER                                        
198900     CALL CBLTDLI USING DLET KATH-PCB DLI-IO-AREA                         
199000     MOVE KATH-STATUS-CODE TO STATUS-WS                                   
199100     PERFORM IMS-STATUSKONTROLL                                           
199200     .                                                                    
199300     EJECT                                                                
199400 IMS-GET-WDGX1212 SECTION.                                                
199500     SKIP2                                                                
199600     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-1211-X  ')'                   
199700          DELIMITED BY SIZE INTO SSA1                                     
199800     STRING 'WDGX1212(IDMODELL =' W-IDMODELL-1212-X ')'                   
199900          DELIMITED BY SIZE INTO SSA2                                     
200000     MOVE '  GE' TO GODK-STATUSKODER                                      
200100     CALL CBLTDLI USING GU 1212-PCB DLI-IO-AREA SSA1 SSA2                 
200200     MOVE 1212-STATUS-CODE TO STATUS-WS                                   
200300     PERFORM IMS-STATUSKONTROLL                                           
200400     .                                                                    
200500     EJECT                                                                
200600 IMS-GET-WDGX1214 SECTION.                                                
200700     SKIP2                                                                
200800     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-1213-X  ')'                   
200900          DELIMITED BY SIZE INTO SSA1                                     
201000     STRING 'WDGX1214(IDVARIAN =' W-IDVARIANT-1214-X ')'                  
201100          DELIMITED BY SIZE INTO SSA2                                     
201200     MOVE '  GE' TO GODK-STATUSKODER                                      
201300     CALL CBLTDLI USING GU 1214-PCB DLI-IO-AREA SSA1 SSA2                 
201400     MOVE 1214-STATUS-CODE TO STATUS-WS                                   
201500     PERFORM IMS-STATUSKONTROLL                                           
201600     .                                                                    
201700     EJECT                                                                
201800 IMS-GET-WDGX1216 SECTION.                                                
201900     SKIP2                                                                
202000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-1215-X ')'                    
202100          DELIMITED BY SIZE INTO SSA1                                     
202200     STRING 'WDGX1216(IDVARIAN =' W-IDVARIANT-1216-X ')'                  
202300          DELIMITED BY SIZE INTO SSA2                                     
202400     MOVE '  GE' TO GODK-STATUSKODER                                      
202500     CALL CBLTDLI USING GU 1216-PCB DLI-IO-AREA SSA1 SSA2                 
202600     MOVE 1216-STATUS-CODE TO STATUS-WS                                   
202700     PERFORM IMS-STATUSKONTROLL                                           
202800     .                                                                    
202900     EJECT                                                                
203000 IMS-GET-WDGX1218 SECTION.                                                
203100     SKIP2                                                                
203200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-1217-X  ')'                   
203300          DELIMITED BY SIZE INTO SSA1                                     
203400     STRING 'WDGX1218(WDGXKEY  =' W-WDGXKEY-1218-X ')'                    
203500          DELIMITED BY SIZE INTO SSA2                                     
203600     MOVE '  GE' TO GODK-STATUSKODER                                      
203700     CALL CBLTDLI USING GU 1218-PCB DLI-IO-AREA SSA1 SSA2                 
203800     MOVE 1218-STATUS-CODE TO STATUS-WS                                   
203900     PERFORM IMS-STATUSKONTROLL                                           
204000     .                                                                    
204100     EJECT                                                                
204200 IMS-STATUSKONTROLL SECTION.                                              
204300     SKIP2                                                                
204400     SET STATUS-IX TO 1                                                   
204500     SEARCH GODK-STATUS                                                   
204600       AT END                                                             
204700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
204800         DELIMITED BY SIZE INTO FELTEXT                                   
204900         CALL FELLOG                                                      
205000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
205100         CONTINUE                                                         
205200     END-SEARCH                                                           
205300     .                                                                    
