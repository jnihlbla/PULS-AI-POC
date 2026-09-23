000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4041300.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   96/04/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KUNDREGISTER.GODSMOTTAGARE INFORMATION 2.                        
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WLGMTA (WDB2)                              
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T413                                              
001400*        MID:         W4I41301                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O41301                                            
001800*    E-TRACKER: 7450328  HÖST -08  VOHF                                   
001900*                                                                         
002000*    E-TRACKER: 8081720  ADD FLVOHF-KL(*) FOR EACH ORDER CLASS            
002100*                                                                         
002200*    E-TRACKER: 10254592 2015  DECOMISSION VOHF                           
002300*                                                                         
002400*    E-TRACKER: 10257039 2016  RFS DATE CALC VALUES FOR DIFF DC'S         
002500*                                                                         
002600                                                                          
002700                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(08)   VALUE 'W4041300'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003710 77  CURRENT-SECTION             PIC X(16) VALUE SPACE.                   
003720 77  CURRENT-IMS-SECTION         PIC X(16) VALUE SPACE.                   
003800                                                                          
004800 77  INDX3                       PIC 9       VALUE ZERO.                  
004801 77  INDX3-MAX                   PIC 9       VALUE 3.                     
004802 77  INDX4                       PIC 9       VALUE ZERO.                  
004803 77  INDX4-MAX                   PIC 9       VALUE 4.                     
004810                                                                          
004820 77  YES                         PIC X       VALUE 'Y'.                   
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 01  ALL-PLUS.                                                            
005200     03 FILLER                   PIC X(20)   VALUE ALL '+'.               
005500                                                                          
007600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007700                                                                          
010200 77  UPD-SW                      PIC X       VALUE 'N'.                   
010300     88  UPPDATERING-GJORD                   VALUE 'J'.                   
010400     88  INGEN-UPPDATERING-GJORD             VALUE 'N'.                   
010500                                                                          
011000 77  BORTTAG-SW                  PIC X       VALUE 'J'.                   
011100     88  BORTTAG-OK                          VALUE 'J'.                   
011200     88  BORTTAG-EJ-OK                       VALUE 'N'.                   
011300                                                                          
011800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
011900     88  ALLT-OK                             VALUE 'J'.                   
012000                                                                          
012100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
012200     88  INDATA-OK                           VALUE 'J'.                   
012300     88  INDATA-FEL                          VALUE 'N'.                   
012400                                                                          
012500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
012600     88  NYCKLAR-OK                          VALUE 'J'.                   
012700     88  NYCKLAR-FEL                         VALUE 'N'.                   
012701                                                                          
012710 77  IDDC-SW                     PIC X       VALUE 'J'.                   
012720     88  IDDC-OK                             VALUE 'J'.                   
012730     88  IDDC-FEL                            VALUE 'N'.                   
012740                                                                          
011800 77  WS-FLAUTREM-SW              PIC X       VALUE 'J'.                   
011900     88  WS-FLAUTREM-OK                      VALUE 'J'.                   
011200     88  WS-FLAUTREM-EJ-OK                   VALUE 'N'.                   
012800                                                                          
013200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013300     88  EGEN-MID                            VALUE '4413'.                
013400     88  GODK-MID                            VALUE '4411' '4412'          
013500                                                   '4413' '4414'          
013600                                                   '4415' '4416'          
013700                                                   '4417' '4418'          
013800                                                   '4419'.                
013900     88  HELP-MID                            VALUE '0551'.                
014000                                                                          
014010                                                                          
014100*      --- VALID IDDC CODES                                               
014200*                                                                         
014300*01    -COPY WWDCKONS                                                     
014400*                                                                         
014500*01    -COPY WWDC99                                                       
014600                                                                          
014700*--------- FÄLT FÖR REDIGERING AV COD                                     
014800 01  W-TICOD.                                                             
014900     03  W-TICOD-X               PIC X(7).                                
015000 01  W-TICOD-RED.                                                         
015100     03  W-TICOD-RED-1           PIC X.                                   
015200     03  W-TICOD-RED-2           PIC X(2).                                
015300     03  W-TICOD-RED-3           PIC X.                                   
015400     03  W-TICOD-RED-4           PIC X(2).                                
015500 01  W-TICODDAT-X.                                                        
015600     03  W-TICODDAT-N            PIC 9(6).                                
015700                                                                          
015710                                                                          
015800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015900 01  GENERELLA-SUBPROGRAM.                                                
016000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016600                                                                          
016610                                                                          
016700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
016800*01 -COPY WMEDAREA                                                        
016900                                                                          
017000 01  MESSAGE-CODES.                                                       
017100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
017200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
017300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
017400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
017600     03  ERR-BO-EXISTS           PIC X(3)    VALUE '361'.                 
017700     03  ERR-ORDER-EXISTS        PIC X(3)    VALUE '366'.                 
017710     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
017720     03  ERR-WRONG-FIELD         PIC X(3)    VALUE '409'.                 
017800                                                                          
017900                                                                          
018000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
018100*                                                                         
018200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
018300                                                                          
018400*01 -COPY WMSGINIT                                                        
018500                                                                          
018600                                                                          
019200 01  FILLER                      PIC X(8)    VALUE 'WORKAREA'.            
019300*01   -COPY  WORKAREA                                                     
019400                                                                          
019410                                                                          
019500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019600*                                                                         
019700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019800                                                                          
019900*01  MID -COPY W4I41301                                                   
020000                                                                          
020010                                                                          
020100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020200                                                                          
020300*01  -COPY WMSGAREA                                                       
020400                                                                          
020500     03  MOD REDEFINES MSG-AREA.                                          
020600*      05  -COPY W4O41301                                                 
020700                                                                          
020710                                                                          
020800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020900                                                                          
021000*01  -COPY WMFSAREA                                                       
021100                                                                          
021200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021300*                                                                         
021400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021500                                                                          
021600 01  NYCKLAR-TILL-DLI.                                                    
021700     03  W-IDGMT-X.                                                       
021800         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
021900         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
022000                                                                          
022100     03  W-WDB301KY-X.                                                    
022200         05  W-IDDC-WDB3         PIC X(2)    VALUE SPACE.                 
022300         05  W-IDDISTR-WDB3      PIC S9(5)   VALUE ZERO COMP-3.           
022400         05  W-IDKUNDNR-WDB3     PIC S9(7)   VALUE ZERO COMP-3.           
022500                                                                          
022600     03  W-WDB301KY-DEF-X.                                                
022700         05  W-IDDC-WDB3-DEF     PIC X(2)    VALUE SPACE.                 
022800         05  W-IDDISTR-WDB3-DEF  PIC S9(5)   VALUE ZERO COMP-3.           
022900         05  W-IDKUNDNR-WDB3-DEF PIC S9(7) VALUE +9999999 COMP-3.         
023000                                                                          
023100     03  W-WDB501KY-X.                                                    
023200         05  W-IDDC-WDB5         PIC X(2)    VALUE SPACE.                 
023300         05  W-IDDISTR-WDB5      PIC S9(5)   VALUE ZERO COMP-3.           
023400         05  W-IDKUNDNR-WDB5     PIC S9(7)   VALUE ZERO COMP-3.           
023500                                                                          
023600     03  W-WDB101KY-X.                                                    
023700         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
023800         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
023900                                                                          
024000     03  W-IDDC-B6-X.                                                     
024100         05 W-IDDC-B6                  PIC X(2).                          
024200                                                                          
026500     03  W-IDORDER-X.                                                     
026600         05 W-IDORDER            PIC S9(7)   COMP-3.                      
026700                                                                          
026800     03  W-IDDC-Q2-X.                                                     
026900         05 W-IDDC-Q2            PIC X(2).                                
027000                                                                          
027100                                                                          
027200*    --- STATUS-KOD FRÅN IMS                                              
027300 01  STATUS-WS                   PIC XX.                                  
027400     88  SEGMENT-FINNS                       VALUE '  '.                  
027500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
027800                                                                          
027900 01  GODK-STATUSKODER.                                                    
028000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028100                                                                          
028200 01  ALL-SSA.                                                             
028210     03 SSA1                     PIC X(121).                              
028300     03 SSA2                     PIC X(64).                               
028400                                                                          
028410                                                                          
028500*    --- IMS FUNKTIONSKODER                                               
028600*01  -COPY W0003                                                          
028700                                                                          
028710                                                                          
028800*    ---  DLI INPUT-OUTPUT AREA                                           
028900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
029000                                                                          
029100 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
029200 01  DLI-IO-AREA.                                                         
029300     03  WLGMTA01.                                                        
029400*        05  -COPY WDB201  -PRE GMTA-                                     
029500                                                                          
029600                                                                          
029700 01  FILLER                      PIC X(16)   VALUE 'WDB301-AREA'.         
029800 01  DLI-IO-AREA-1.                                                       
029900     03  IO-AREA-1               PIC X(300)  VALUE SPACE.                 
030000                                                                          
030100     03  WLGMTB01 REDEFINES IO-AREA-1.                                    
030200*        05  -COPY WDB301  -PRE GMTB-                                     
030300                                                                          
030400                                                                          
030500 01  FILLER                      PIC X(16)   VALUE 'WDB501-AREA'.         
030600 01  DLI-IO-AREA-2.                                                       
030700     03  IO-AREA-2               PIC X(300)  VALUE SPACE.                 
030800                                                                          
030900     03  WLGMTC01 REDEFINES IO-AREA-2.                                    
031000*        05  -COPY WDB501  -PRE GMTC-                                     
031100                                                                          
031200                                                                          
031300 01  DLI-IO-AREA-3.                                                       
031400     03  IO-AREA-3               PIC X(300)  VALUE SPACE.                 
031500                                                                          
031600     03  WLWDB101 REDEFINES IO-AREA-3.                                    
031700*        05  -COPY WDB101  -PRE WDB1-                                     
031800                                                                          
031900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
032000 01  DLI-IO-AREA-B601.                                                    
032100*    03  -COPY WDB601                                                     
032200                                                                          
033700                                                                          
033800 LINKAGE SECTION.                                                         
033900*01  -COPY W0009   -PRE MSG-                                              
034000*01  -COPY W0008   -PRE USEA-                                             
034100     05  FILLER                  PIC X.                                   
034200                                                                          
034300*01  -COPY W0008  -PRE GMTA-                                              
034400     05  FILLER                  PIC X.                                   
034500                                                                          
034600*01  -COPY W0008  -PRE GMTB-                                              
034700     05  FILLER                  PIC X.                                   
034800                                                                          
034900*01  -COPY W0008  -PRE GMTC-                                              
035000     05  FILLER                  PIC X.                                   
035100                                                                          
035200*01  -COPY W0008  -PRE WDB1-                                              
035300     05  FILLER                  PIC X.                                   
035400                                                                          
035500*01  -COPY W0008  -PRE WDB6-                                              
035600     05  FILLER                  PIC X.                                   
035700                                                                          
036700                                                                          
036800 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB GMTA-PCB GMTB-PCB             
036902                           GMTC-PCB WDB1-PCB WDB6-PCB.                    
037100 MAIN SECTION.                                                            
037200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB GMTA-PCB GMTB-PCB             
037302                           GMTC-PCB WDB1-PCB WDB6-PCB.                    
037500                                                                          
037600     PERFORM IMS-GET-MSG                                                  
037700     IF SEGMENT-FINNS                                                     
037800        PERFORM A-INIT                                                    
037900        PERFORM B-KOLLA-NYCKLAR                                           
038000        IF NYCKLAR-OK                                                     
038100           IF MFS-UPDATE OR MFS-UPD-V                                     
038200              PERFORM G-KOLLA-INPUT                                       
038300              IF INDATA-OK                                                
038400                 PERFORM H-UPPDATERA                                      
038500              END-IF                                                      
038600           ELSE                                                           
038700              IF MFS-FIRST                                                
038800                 PERFORM C-FOERSTA-SIDA                                   
038900              ELSE                                                        
039000                 PERFORM E-SAMMA-SIDA                                     
039100              END-IF                                                      
039200           END-IF                                                         
039300           IF ALLT-OK                                                     
039400              PERFORM F-LAES-VISA-INFO                                    
039500           END-IF                                                         
039600        END-IF                                                            
039700                                                                          
039900        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O41301 + 4                     
040000        PERFORM IMS-INSERT-MSG                                            
040100     END-IF                                                               
040200                                                                          
040300     MOVE ZERO TO RETURN-CODE                                             
040400     GOBACK                                                               
040500     .                                                                    
040600                                                                          
040610                                                                          
040700 A-INIT SECTION.                                                          
040710     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
040800                                                                          
040900     IF MSG-DUBBLA-TRANSKODER                                             
041000        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I41301                
041100        MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                
041200        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
041300     ELSE                                                                 
041400        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I41301                 
041500        MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                
041600        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
041700     END-IF                                                               
041800                                                                          
041900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
042000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
042100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
042200                                                                          
042300     MOVE LOW-VALUE  TO MSG-AREA                                          
042400     MOVE 'W4O413N1' TO MFS-IDMOD                                         
042500     MOVE '4413'     TO MOD-IDTRANS                                       
042600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
042700                                                                          
042800     IF EGEN-MID OR HELP-MID                                              
042900        CONTINUE                                                          
043000     ELSE                                                                 
043100        MOVE SPACE TO MFS-KDTRTYP                                         
043200        MOVE '7'   TO MFS-IDPFK                                           
043300     END-IF                                                               
043400                                                                          
043500     MOVE 'GB' TO MED-IDSKYLT                                             
043700     .                                                                    
043800                                                                          
043810                                                                          
043900 B-KOLLA-NYCKLAR SECTION.                                                 
043910     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
044000                                                                          
044100     MOVE ALL '+'            TO MSGI-WMSGINIT                             
044200     MOVE '001'              TO MSGI-KDCALL                               
044300     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
044400     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
044500     MOVE '4413'             TO MSGI-IDTRANS                              
044600     IF EGEN-MID                                                          
044700        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
044800        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
044900     END-IF                                                               
045000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
045100     MOVE MSGI-IDLAND-SPR    TO MED-IDSKYLT                               
045200                                                                          
045300     MOVE JA TO NYCKLAR-SW                                                
045400                                                                          
045500                                                                          
045600*    -- KONTROLL AV IDDISTR                                               
045700     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
045800                                                                          
045900     IF MID-IDDISTR-IN NOT = ALL '+'                                      
046000        MOVE '7'          TO MFS-IDPFK                                    
046100        MOVE SPACE        TO MFS-KDTRTYP                                  
046200     END-IF                                                               
046300     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
046400     IF MSGI-IDDISTR NUMERIC                                              
046500        MOVE MSGI-IDDISTR TO W-IDDISTR                                    
046600                             W-IDDISTR-WDB3                               
046700                             W-IDDISTR-WDB3-DEF                           
047200     ELSE                                                                 
047300        MOVE NEJ TO NYCKLAR-SW                                            
047400     END-IF                                                               
047500                                                                          
047600*    -- KONTROLL AV IDKUNDNR                                              
047700     MOVE MFS-RENSA-FAELT  TO MOD-IDKUNDNR-IN                             
047800                                                                          
047900     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
048000        MOVE '7'           TO MFS-IDPFK                                   
048100        MOVE SPACE         TO MFS-KDTRTYP                                 
048200     END-IF                                                               
048300     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
048400     IF MSGI-IDKUNDNR NUMERIC                                             
048500        MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                  
048600                              W-IDKUNDNR-WDB3                             
049100     ELSE                                                                 
049200       MOVE NEJ TO NYCKLAR-SW                                             
049300     END-IF                                                               
049400                                                                          
049500     IF GODK-MID OR NYCKLAR-OK                                            
049600        MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                        
049700        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
049800        MOVE MSGI-IDKUNDNR       TO MOD-IDKUNDNR-UT                       
049900        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
050000     ELSE                                                                 
050100        MOVE MFS-RENSA-FAELT     TO MOD-IDDISTR-UT                        
050200                                    MOD-IDKUNDNR-UT                       
050300     END-IF                                                               
050400                                                                          
050500     IF NYCKLAR-FEL                                                       
050600        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
050700        CALL WMEDKONV USING MED-WMEDAREA                                  
050800        MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                
050900        PERFORM MFS-RENSA-FAELT-IN                                        
051000        PERFORM MFS-RENSA-FAELT-UT                                        
051100     END-IF                                                               
051200     .                                                                    
051300                                                                          
051310                                                                          
051400 C-FOERSTA-SIDA SECTION.                                                  
051410     MOVE 'C-FOERSTA-SIDA  ' TO CURRENT-SECTION                           
051500                                                                          
051600     PERFORM MFS-RENSA-FAELT-IN                                           
051700     .                                                                    
051800                                                                          
051810                                                                          
051900 E-SAMMA-SIDA SECTION.                                                    
051910     MOVE 'E-SAMMA-SIDA    ' TO CURRENT-SECTION                           
052000                                                                          
052100     PERFORM IMS-GHU-WDB201                                               
056700                                                                          
056800     IF EGEN-MID OR HELP-MID                                              
056900        IF MID-INPUT-U-TRANS = ALL '+' AND                                
056910           MID-INPUT-V-TRANS = ALL '+'                                    
057000           PERFORM MFS-RENSA-FAELT-IN                                     
057100           MOVE JA TO ALLT-SW                                             
057200        ELSE                                                              
057300           MOVE NEJ TO ALLT-SW                                            
057400           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
057500           CALL WMEDKONV USING MED-WMEDAREA                               
057600           MOVE MED-MFSINF     TO MOD-TEMFSINF                            
057700           PERFORM MFS-ROER-EJ-FAELT-IN                                   
057800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
057900           PERFORM EA-MID-INDATA-TILL-MOD                                 
058000        END-IF                                                            
058100     ELSE                                                                 
058200        PERFORM MFS-RENSA-FAELT-IN                                        
058300     END-IF                                                               
058400     .                                                                    
058500                                                                          
058510                                                                          
058600 EA-MID-INDATA-TILL-MOD SECTION.                                          
058610     MOVE 'EA-MID-TILL-MOD ' TO CURRENT-SECTION                           
058700                                                                          
061400     IF MID-IDDC-RET  NOT = ALL '+'                                       
061500        MOVE MID-IDDC-RET          TO MOD-IDDC-RET                        
061600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-RET-ATTR                   
061700     ELSE                                                                 
061800        MOVE MFS-ROER-EJ-FAELT     TO MOD-IDDC-RET                        
061900     END-IF                                                               
062000                                                                          
062800     IF MID-FLRETFG  NOT = ALL '+'                                        
062900        IF MID-FLRETFG = 'J'                                              
063000           MOVE 'Y'                TO MOD-FLRETFG                         
063100        ELSE                                                              
063200           MOVE MID-FLRETFG        TO MOD-FLRETFG                         
063300        END-IF                                                            
063400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLRETFG-ATTR                    
063500     ELSE                                                                 
063600        MOVE MFS-ROER-EJ-FAELT     TO MOD-FLRETFG                         
063700     END-IF                                                               
063701                                                                          
063710     IF MID-FLAUTREM NOT = ALL '+'                                        
063720        IF MID-FLAUTREM = 'J'                                             
063730           MOVE 'Y'                TO MOD-FLAUTREM                        
063740        ELSE                                                              
063750           MOVE MID-FLAUTREM       TO MOD-FLAUTREM                        
063760        END-IF                                                            
063770        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAUTREM-ATTR                   
063780     ELSE                                                                 
063790        MOVE MFS-ROER-EJ-FAELT     TO MOD-FLAUTREM                        
063791     END-IF                                                               
063800                                                                          
063900     IF MID-FLOKFAK-G  NOT = ALL '+'                                      
064000        IF  MID-FLOKFAK-G = 'J'                                           
064100            MOVE 'Y'               TO MOD-FLOKFAK-G                       
064200        ELSE                                                              
064300            MOVE MID-FLOKFAK-G     TO MOD-FLOKFAK-G                       
064400        END-IF                                                            
064500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLOKFAK-G-ATTR                  
064600     ELSE                                                                 
064700        MOVE MFS-ROER-EJ-FAELT     TO MOD-FLOKFAK-G                       
064800     END-IF                                                               
064900                                                                          
065000     IF MID-FLOKFAK-K  NOT = ALL '+'                                      
065100        IF  MID-FLOKFAK-K = 'J'                                           
065200            MOVE 'Y'               TO MOD-FLOKFAK-K                       
065300        ELSE                                                              
065400            MOVE MID-FLOKFAK-K     TO MOD-FLOKFAK-K                       
065500        END-IF                                                            
065600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLOKFAK-K-ATTR                  
065700     ELSE                                                                 
065800        MOVE MFS-ROER-EJ-FAELT     TO MOD-FLOKFAK-K                       
065900     END-IF                                                               
066000                                                                          
066100     IF MID-FLOKFAK-N  NOT = ALL '+'                                      
066200        IF  MID-FLOKFAK-N = 'J'                                           
066300            MOVE 'Y'               TO MOD-FLOKFAK-N                       
066400        ELSE                                                              
066500            MOVE MID-FLOKFAK-N     TO MOD-FLOKFAK-N                       
066600        END-IF                                                            
066700        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLOKFAK-N-ATTR                  
066800     ELSE                                                                 
066900        MOVE MFS-ROER-EJ-FAELT     TO MOD-FLOKFAK-N                       
067000     END-IF                                                               
067100                                                                          
067200     IF MID-FLOKFAK-R  NOT = ALL '+'                                      
067300        IF  MID-FLOKFAK-R = 'J'                                           
067400            MOVE 'Y'               TO MOD-FLOKFAK-R                       
067500        ELSE                                                              
067600            MOVE MID-FLOKFAK-R     TO MOD-FLOKFAK-R                       
067700        END-IF                                                            
067800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLOKFAK-R-ATTR                  
067900     ELSE                                                                 
068000        MOVE MFS-ROER-EJ-FAELT     TO MOD-FLOKFAK-R                       
068100     END-IF                                                               
068200                                                                          
068300     IF MID-KDGENFAK  NOT = ALL '+'                                       
068400        MOVE MID-KDGENFAK          TO MOD-KDGENFAK                        
068500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDGENFAK-ATTR                   
068600     ELSE                                                                 
068700        MOVE MFS-ROER-EJ-FAELT     TO MOD-KDGENFAK                        
068800     END-IF                                                               
068810                                                                          
068820     IF MID-FLLDCKND  NOT = ALL '+'                                       
068830        MOVE MID-FLLDCKND          TO MOD-FLLDCKND                        
068840        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLLDCKND-ATTR                   
068850     ELSE                                                                 
068860        MOVE MFS-ROER-EJ-FAELT     TO MOD-FLLDCKND                        
068870     END-IF                                                               
068900                                                                          
069000     IF MID-KVDAGAR-SDC NOT = ALL '+'                                     
069100        MOVE MID-KVDAGAR-SDC       TO MOD-KVDAGAR-SDC                     
069200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVDAGAR-SDC-ATTR                
069300     ELSE                                                                 
069400        MOVE MFS-ROER-EJ-FAELT     TO MOD-KVDAGAR-SDC                     
069500     END-IF                                                               
069600                                                                          
069700     IF MID-KVDAGAR-CDC NOT = ALL '+'                                     
069800        MOVE MID-KVDAGAR-CDC       TO MOD-KVDAGAR-CDC                     
069900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVDAGAR-CDC-ATTR                
070000     ELSE                                                                 
070100        MOVE MFS-ROER-EJ-FAELT     TO MOD-KVDAGAR-CDC                     
070200     END-IF                                                               
070210                                                                          
070220     IF MID-FLRETUR NOT = ALL '+'                                         
070230        MOVE MID-FLRETUR           TO MOD-FLRETUR                         
070240        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLRETUR-ATTR                    
070250     ELSE                                                                 
070260        MOVE MFS-ROER-EJ-FAELT     TO MOD-FLRETUR                         
070270     END-IF                                                               
070300                                                                          
070310     MOVE 1 TO INDX3                                                      
070320     PERFORM UNTIL INDX3 > INDX3-MAX                                      
070400        IF MID-IDDC-RET72(INDX3) NOT = ALL '+'                            
070500           MOVE MID-IDDC-RET72(INDX3)                                     
070501                                   TO MOD-IDDC-RET72(INDX3)               
070510           MOVE MFS-ADD-LAES-IN-FAELT                                     
070511                                   TO MOD-IDDC-RET72-ATTR(INDX3)          
070520        ELSE                                                              
070530           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDDC-RET72-ATTR(INDX3)          
070540        END-IF                                                            
070541        ADD 1 TO INDX3                                                    
070542     END-PERFORM                                                          
070543                                                                          
070544     MOVE 1 TO INDX4                                                      
070545     PERFORM UNTIL INDX4 > INDX4-MAX                                      
070546        IF MID-IDDC-RFS(INDX4) NOT = ALL '+'                              
070547           MOVE MID-IDDC-RFS(INDX4)                                       
070548                                   TO MOD-IDDC-RFS(INDX4)                 
070549           MOVE MFS-ADD-LAES-IN-FAELT                                     
070550                                   TO MOD-IDDC-RFS-ATTR(INDX4)            
070551        ELSE                                                              
070552           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDDC-RFS-ATTR(INDX4)            
070553        END-IF                                                            
070554        IF MID-KVDAGAR-RFS(INDX4) NOT = ALL '+'                           
070555           MOVE MID-KVDAGAR-RFS(INDX4)                                    
070556                                   TO MOD-KVDAGAR-RFS(INDX4)              
070557           MOVE MFS-ADD-LAES-IN-FAELT                                     
070558                                   TO MOD-KVDAGAR-RFS-ATTR(INDX4)         
070559        ELSE                                                              
070560           MOVE MFS-ROER-EJ-FAELT  TO MOD-KVDAGAR-RFS-ATTR(INDX4)         
070561        END-IF                                                            
070562        ADD 1 TO INDX4                                                    
070563     END-PERFORM                                                          
070564                                                                          
070565     IF MID-KVDAGAR-RFS-DEF NOT = ALL '+'                                 
070566        MOVE MID-KVDAGAR-RFS-DEF   TO MOD-KVDAGAR-RFS-DEF                 
070567        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVDAGAR-RFS-DEF-ATTR            
070568     ELSE                                                                 
070569        MOVE MFS-ROER-EJ-FAELT     TO MOD-KVDAGAR-RFS-DEF                 
070570     END-IF                                                               
070571                                                                          
070572     IF MID-FLKVBRYT-ORDKL1 NOT = ALL '+'                                 
070573        MOVE MID-FLKVBRYT-ORDKL1   TO MOD-FLKVBRYT-ORDKL1                 
070574        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKVBRYT-ORDKL1-ATTR            
070575     ELSE                                                                 
070576        MOVE MFS-ROER-EJ-FAELT     TO MOD-FLKVBRYT-ORDKL1                 
070577     END-IF                                                               
070578                                                                          
070579     IF MID-FLKVBRYT-ORDKL2 NOT = ALL '+'                                 
070580        MOVE MID-FLKVBRYT-ORDKL2   TO MOD-FLKVBRYT-ORDKL2                 
070581        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKVBRYT-ORDKL2-ATTR            
070582     ELSE                                                                 
070583        MOVE MFS-ROER-EJ-FAELT     TO MOD-FLKVBRYT-ORDKL2                 
070584     END-IF                                                               
070585                                                                          
070586     IF MID-FLKVBRYT-ORDKL3 NOT = ALL '+'                                 
070587        MOVE MID-FLKVBRYT-ORDKL3   TO MOD-FLKVBRYT-ORDKL3                 
070588        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKVBRYT-ORDKL3-ATTR            
070589     ELSE                                                                 
070590        MOVE MFS-ROER-EJ-FAELT     TO MOD-FLKVBRYT-ORDKL3                 
070591     END-IF                                                               
070592                                                                          
070593     IF MID-FLKVBRYT-ORDKL4 NOT = ALL '+'                                 
070594        MOVE MID-FLKVBRYT-ORDKL4   TO MOD-FLKVBRYT-ORDKL4                 
070595        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKVBRYT-ORDKL4-ATTR            
070596     ELSE                                                                 
070597        MOVE MFS-ROER-EJ-FAELT     TO MOD-FLKVBRYT-ORDKL4                 
070598     END-IF                                                               
070599                                                                          
070610     .                                                                    
070700                                                                          
070710                                                                          
070800 F-LAES-VISA-INFO SECTION.                                                
070810     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
070900                                                                          
071000     PERFORM IMS-GHU-WDB201                                               
071100                                                                          
071200     IF SEGMENT-SAKNAS                                                    
071300* FLYTTA LÄMPLIGT FELMEDDELANDE                                           
071400*       CALL WMEDKONV USING MED-WMEDAREA                                  
071500*       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
071600        MOVE 'GOODS RECEIVER MISSING' TO MOD-TEMFSFEL                     
071700        PERFORM MFS-RENSA-FAELT-UT                                        
071800     ELSE                                                                 
071810                                                                          
071811       MOVE GMTA-GMT-IDDC-RET         TO MOD-IDDC-RET                     
071820       IF GMTA-GMT-FLRETFG = 'J'                                          
071830         MOVE 'Y'                     TO MOD-FLRETFG                      
071840       ELSE                                                               
071850         MOVE GMTA-GMT-FLRETFG        TO MOD-FLRETFG                      
071860       END-IF                                                             
071861                                                                          
071862       MOVE GMTA-GMT-IDDC-RET         TO MOD-IDDC-RET                     
071863       IF GMTA-GMT-FLAUTREM = 'J'                                         
071864         MOVE 'Y'                     TO MOD-FLAUTREM                     
071865       ELSE                                                               
071866         MOVE GMTA-GMT-FLAUTREM       TO MOD-FLAUTREM                     
071867       END-IF                                                             
071868                                                                          
071869       IF  GMTA-GMT-FLOKFAK-G = 'J'                                       
071870           MOVE 'Y'                   TO MOD-FLOKFAK-G                    
071871       ELSE                                                               
071872           MOVE GMTA-GMT-FLOKFAK-G    TO MOD-FLOKFAK-G                    
071873       END-IF                                                             
071874       IF  GMTA-GMT-FLOKFAK-K = 'J'                                       
071875           MOVE 'Y'                   TO MOD-FLOKFAK-K                    
071876       ELSE                                                               
071877           MOVE GMTA-GMT-FLOKFAK-K    TO MOD-FLOKFAK-K                    
071878       END-IF                                                             
071879       IF  GMTA-GMT-FLOKFAK-N = 'J'                                       
071880           MOVE 'Y'                   TO MOD-FLOKFAK-N                    
071881       ELSE                                                               
071882           MOVE GMTA-GMT-FLOKFAK-N    TO MOD-FLOKFAK-N                    
071883       END-IF                                                             
071884       IF  GMTA-GMT-FLOKFAK-R = 'J'                                       
071885           MOVE 'Y'                   TO MOD-FLOKFAK-R                    
071886       ELSE                                                               
071887           MOVE GMTA-GMT-FLOKFAK-R    TO MOD-FLOKFAK-R                    
071888       END-IF                                                             
071889       MOVE GMTA-GMT-KDGENFAK            TO MOD-KDGENFAK                  
071890                                                                          
071900       IF  GMTA-GMT-FLCOD = 'J'                                           
072000           MOVE 'Y'                   TO MOD-FLCOD                        
072100       ELSE                                                               
072200           MOVE GMTA-GMT-FLCOD        TO MOD-FLCOD                        
072300       END-IF                                                             
072400       MOVE GMTA-GMT-TISTADAT-COD        TO W-TICODDAT-N                  
072500       MOVE GMTA-GMT-TISTATID-COD        TO W-TICOD-X                     
072600       MOVE W-TICODDAT-X                 TO MSGI-TILOKDAT                 
072700       MOVE W-TICOD-X(2:4)               TO MSGI-TILOKTID                 
072800       MOVE '011'                        TO MSGI-KDCALL                   
072900       MOVE MSG-SIGNON-USERID            TO MSGI-IDUSER                   
073000                                            MSGI-IDLTERM-USER             
073100       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
073200       MOVE MSGI-TILOKDAT                TO W-TICODDAT-X                  
073300       MOVE MSGI-TILOKTID                TO W-TICOD-X(2:4)                
073400       MOVE W-TICODDAT-N                 TO MOD-TISTADAT-COD              
073500       MOVE SPACE                        TO W-TICOD-RED-1                 
073600       MOVE W-TICOD-X (2:2)              TO W-TICOD-RED-2                 
073700       MOVE ':'                          TO W-TICOD-RED-3                 
073800       MOVE W-TICOD-X (4:2)              TO W-TICOD-RED-4                 
073900       MOVE W-TICOD-RED                  TO MOD-TISTATID-COD              
074000       MOVE GMTA-GMT-TISTODAT-COD        TO W-TICODDAT-N                  
074100       MOVE GMTA-GMT-TISTOTID-COD        TO W-TICOD-X                     
074200       MOVE W-TICODDAT-X                 TO MSGI-TILOKDAT                 
074300       MOVE W-TICOD-X(2:4)               TO MSGI-TILOKTID                 
074400       MOVE '011'                        TO MSGI-KDCALL                   
074500       MOVE MSG-SIGNON-USERID            TO MSGI-IDUSER                   
074600                                            MSGI-IDLTERM-USER             
074700       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
074800       MOVE MSGI-TILOKDAT                TO W-TICODDAT-X                  
074900       MOVE MSGI-TILOKTID                TO W-TICOD-X(2:4)                
075000       MOVE W-TICODDAT-N                 TO MOD-TISTODAT-COD              
075100       MOVE SPACE                        TO W-TICOD-RED-1                 
075200       MOVE W-TICOD-X (2:2)              TO W-TICOD-RED-2                 
075300       MOVE ':'                          TO W-TICOD-RED-3                 
075400       MOVE W-TICOD-X (4:2)              TO W-TICOD-RED-4                 
075500       MOVE W-TICOD-RED                  TO MOD-TISTOTID-COD              
075510                                                                          
078600       MOVE GMTA-GMT-TIFAKT              TO MOD-TIFAKT                    
078700       MOVE GMTA-GMT-TISTADAT            TO MOD-TISTADAT                  
078800       MOVE GMTA-GMT-TISTODAT            TO MOD-TISTODAT                  
079200       PERFORM FA-VISA-LDC                                                
079300     END-IF                                                               
079400     .                                                                    
079500                                                                          
079600                                                                          
083700 FA-VISA-LDC        SECTION.                                              
083710     MOVE 'FA-VISA-LDC     ' TO CURRENT-SECTION                           
083800                                                                          
083900     IF GMTA-GMT-FLLDCKND = JA                                            
084000        MOVE YES                    TO MOD-FLLDCKND                       
084100     ELSE                                                                 
084200        MOVE GMTA-GMT-FLLDCKND      TO MOD-FLLDCKND                       
084300     END-IF                                                               
084301     MOVE GMTA-GMT-KVDAGAR-SDC      TO MOD-KVDAGAR-SDC                    
084302     MOVE GMTA-GMT-KVDAGAR-CDC      TO MOD-KVDAGAR-CDC                    
084303     IF GMTA-GMT-FLRETUR = JA                                             
084304        MOVE YES                    TO MOD-FLRETUR                        
084305     ELSE                                                                 
084306        MOVE GMTA-GMT-FLRETUR       TO MOD-FLRETUR                        
084307     END-IF                                                               
084310                                                                          
084320     MOVE 1 TO INDX3                                                      
084330     PERFORM UNTIL INDX3 > INDX3-MAX                                      
084340        MOVE GMTA-GMT-IDDC-RET72(INDX3)                                   
084350                                    TO MOD-IDDC-RET72(INDX3)              
084360        ADD 1 TO INDX3                                                    
084370     END-PERFORM                                                          
084380                                                                          
084390     MOVE 1 TO INDX4                                                      
084391     PERFORM UNTIL INDX4 > INDX4-MAX                                      
084392        MOVE GMTA-GMT-IDDC-RFS(INDX4)                                     
084393                                    TO MOD-IDDC-RFS(INDX4)                
084394        MOVE GMTA-GMT-KVDAGAR-RFS(INDX4)                                  
084395                                    TO MOD-KVDAGAR-RFS(INDX4)             
084396        ADD 1 TO INDX4                                                    
084397     END-PERFORM                                                          
084398                                                                          
084399     MOVE GMTA-GMT-KVDAGAR-RFS-DEF  TO MOD-KVDAGAR-RFS-DEF                
084400     IF GMTA-GMT-FLKVBRYT-ORDKL1 = JA                                     
084401        MOVE YES                      TO MOD-FLKVBRYT-ORDKL1              
084402     ELSE                                                                 
084403        MOVE GMTA-GMT-FLKVBRYT-ORDKL1 TO MOD-FLKVBRYT-ORDKL1              
084404     END-IF                                                               
084405     IF GMTA-GMT-FLKVBRYT-ORDKL2 = JA                                     
084406        MOVE YES                      TO MOD-FLKVBRYT-ORDKL2              
084407     ELSE                                                                 
084408        MOVE GMTA-GMT-FLKVBRYT-ORDKL2 TO MOD-FLKVBRYT-ORDKL2              
084409     END-IF                                                               
084410     IF GMTA-GMT-FLKVBRYT-ORDKL3 = JA                                     
084411        MOVE YES                      TO MOD-FLKVBRYT-ORDKL3              
084412     ELSE                                                                 
084413        MOVE GMTA-GMT-FLKVBRYT-ORDKL3 TO MOD-FLKVBRYT-ORDKL3              
084414     END-IF                                                               
084415     IF GMTA-GMT-FLKVBRYT-ORDKL4 = JA                                     
084416        MOVE YES                      TO MOD-FLKVBRYT-ORDKL4              
084417     ELSE                                                                 
084418        MOVE GMTA-GMT-FLKVBRYT-ORDKL4 TO MOD-FLKVBRYT-ORDKL4              
084419     END-IF                                                               
085800                                                                          
087900     .                                                                    
088000                                                                          
088100                                                                          
088200 G-KOLLA-INPUT SECTION.                                                   
088210     MOVE 'G-KOLLA-INPUT   ' TO CURRENT-SECTION                           
088300                                                                          
088400     MOVE JA   TO INDATA-SW                                               
088500     MOVE NEJ  TO BORTTAG-SW                                              
088400     MOVE JA   TO WS-FLAUTREM-SW                                          
088600                                                                          
089000     IF MID-INPUT-U-TRANS = ALL '+' AND                                   
089010        MID-INPUT-V-TRANS = ALL '+'                                       
089000        IF MID-FLAUTREM NOT = ALL '+'                                     
188486           IF MID-FLAUTREM = 'Y' OR 'J' OR 'N'                            
188487              MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAUTREM-ATTR              
088400              MOVE JA   TO INDATA-SW                                      
088400              MOVE JA   TO WS-FLAUTREM-SW                                 
188488           ELSE                                                           
188489              MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAUTREM-ATTR                
188490              MOVE NEJ TO INDATA-SW                                       
188491           END-IF                                                         
089800        ELSE                                                              
089100           MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                      
089200           CALL WMEDKONV USING MED-WMEDAREA                               
089300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
089400           PERFORM MFS-ROER-EJ-FAELT-IN                                   
089500           PERFORM MFS-ROER-EJ-FAELT-UT                                   
089600           MOVE NEJ TO INDATA-SW                                          
089700                       ALLT-SW                                            
089700                       WS-FLAUTREM-SW                                     
115900        END-IF                                                            
089800     ELSE                                                                 
089900        PERFORM IMS-GHU-WDB201                                            
090000        IF SEGMENT-FINNS                                                  
090100           IF MFS-UPDATE                                                  
090101              IF MID-INPUT-V-TRANS = ALL '+'                              
090102                 PERFORM GB-KOLLA-U-DATA                                  
090103              ELSE                                                        
090104                 MOVE ERR-WRONG-FIELD TO MED-IDMFSFEL                     
090105                 CALL WMEDKONV USING MED-WMEDAREA                         
090106                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
090107                 PERFORM MFS-ROER-EJ-FAELT-IN                             
090108                 PERFORM MFS-ROER-EJ-FAELT-UT                             
090109                 PERFORM GA-MARKERA-FEL-V-DATA                            
090110                 MOVE NEJ TO INDATA-SW                                    
090111                             ALLT-SW                                      
090112              END-IF                                                      
090113           ELSE                                                           
090120              PERFORM GB-KOLLA-U-DATA                                     
090130              PERFORM GC-KOLLA-V-DATA                                     
090200           END-IF                                                         
102800                                                                          
114600           IF INDATA-FEL                                                  
114700              MOVE NEJ TO ALLT-SW                                         
114800              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
114900              CALL WMEDKONV USING MED-WMEDAREA                            
115000              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
115100              MOVE MED-MFSINF TO MOD-TEMFSINF                             
115200              PERFORM MFS-ROER-EJ-FAELT-UT                                
115300              PERFORM MFS-ROER-EJ-FAELT-IN                                
115400           END-IF                                                         
115500        ELSE                                                              
115600           MOVE 'SEGMENT-SAKNAS ' TO MOD-TEMFSFEL                         
115700           MOVE NEJ TO INDATA-SW                                          
115800        END-IF                                                            
115900     END-IF                                                               
116000     .                                                                    
187900                                                                          
188000                                                                          
188100 GA-MARKERA-FEL-V-DATA SECTION.                                           
188110     MOVE 'GA-FEL-V-DATA   ' TO CURRENT-SECTION                           
188111                                                                          
188120     IF MID-FLLDCKND NOT = ALL '+'                                        
188190        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLDCKND-ATTR                      
188195     END-IF                                                               
188196                                                                          
188197     IF MID-KVDAGAR-SDC NOT = ALL '+'                                     
188202        MOVE MFS-NUM-FAELT-FEL  TO MOD-KVDAGAR-SDC-ATTR                   
188207     END-IF                                                               
188208                                                                          
188209     IF MID-KVDAGAR-CDC NOT = ALL '+'                                     
188214        MOVE MFS-NUM-FAELT-FEL  TO MOD-KVDAGAR-CDC-ATTR                   
188219     END-IF                                                               
188220                                                                          
188221     IF MID-FLRETUR NOT = ALL '+'                                         
188228        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRETUR-ATTR                       
188233     END-IF                                                               
188234                                                                          
188235     IF MID-IDDC-RET72(1) NOT = ALL '+'                                   
188241        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-RET72-ATTR(1)                 
188246     END-IF                                                               
188247                                                                          
188248     IF MID-IDDC-RET72(2) NOT = ALL '+'                                   
188254        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-RET72-ATTR(2)                 
188259     END-IF                                                               
188260                                                                          
188261     IF MID-IDDC-RET72(3) NOT = ALL '+'                                   
188267        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-RET72-ATTR(3)                 
188272     END-IF                                                               
188286                                                                          
188287     PERFORM                                                              
188288     VARYING INDX4 FROM 1 BY 1                                            
188289        UNTIL INDX4 > INDX4-MAX                                           
188290        IF MID-IDDC-RFS (INDX4) NOT = ALL '+'                             
188301           MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-RFS-ATTR                  
188302                                                  (INDX4)                 
188305        END-IF                                                            
188316                                                                          
188317        IF MID-KVDAGAR-RFS (INDX4) NOT = ALL '+'                          
188325           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVDAGAR-RFS-ATTR               
188326                                                      (INDX4)             
188334        END-IF                                                            
188335     END-PERFORM                                                          
188336                                                                          
188337     IF MID-KVDAGAR-RFS-DEF NOT = ALL '+'                                 
188343        MOVE MFS-NUM-FAELT-FEL TO MOD-KVDAGAR-RFS-DEF-ATTR                
188348     END-IF                                                               
188349                                                                          
188350     IF MID-FLKVBRYT-ORDKL1 NOT = ALL '+'                                 
188357        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKVBRYT-ORDKL1-ATTR               
188362     END-IF                                                               
188363                                                                          
188364     IF MID-FLKVBRYT-ORDKL2 NOT = ALL '+'                                 
188371        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKVBRYT-ORDKL2-ATTR               
188377     END-IF                                                               
188378                                                                          
188379     IF MID-FLKVBRYT-ORDKL3 NOT = ALL '+'                                 
188386        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKVBRYT-ORDKL3-ATTR               
188391     END-IF                                                               
188392                                                                          
188393     IF MID-FLKVBRYT-ORDKL4 NOT = ALL '+'                                 
188400        MOVE MFS-ALFA-FAELT-FEL        TO MOD-FLKVBRYT-ORDKL4-ATTR        
188405     END-IF                                                               
188440     .                                                                    
188441                                                                          
188442                                                                          
188443 GB-KOLLA-U-DATA SECTION.                                                 
188444     MOVE 'GB-KOLLA-U-DATA ' TO CURRENT-SECTION                           
188445                                                                          
188446         IF MID-IDDC-RET  NOT = ALL '+'                                   
188447           MOVE MID-IDDC-RET      TO W-IDDC-B6                            
188448           PERFORM IMS-GU-WDB601                                          
188449           IF  DCS-KDDC = SPACE OR DCS-DDC                                
188450             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-RET-ATTR                 
188451             MOVE NEJ TO INDATA-SW                                        
188452           ELSE                                                           
188453             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-RET-ATTR               
188454           END-IF                                                         
188455         END-IF                                                           
188456                                                                          
188457         IF MID-FLOKFAK-G NOT = ALL '+'                                   
188458           IF MID-FLOKFAK-G = 'Y' OR 'J' OR 'N'                           
188459             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLOKFAK-G-ATTR              
188460*EJ GODKÄNT ATT KUNNA HA BÅDE R-FAKTURA OCH N-FAKTURA                     
188461*TL 021210                                                                
188462             IF MID-FLOKFAK-G = 'Y' OR 'J'                                
188463               IF (MID-FLOKFAK-N = 'Y' OR 'J')     OR                     
188464                  (GMTA-GMT-FLOKFAK-N = 'Y' OR 'J')                       
188465                                                                          
188466                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOKFAK-G-ATTR            
188467                 MOVE NEJ TO INDATA-SW                                    
188468               END-IF                                                     
188469             END-IF                                                       
188470           ELSE                                                           
188471             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOKFAK-G-ATTR                
188472             MOVE NEJ TO INDATA-SW                                        
188473           END-IF                                                         
188474         END-IF                                                           
188475                                                                          
188476         IF MID-FLRETFG NOT = ALL '+'                                     
188477           IF MID-FLRETFG = 'Y' OR 'J' OR 'N'                             
188478             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRETFG-ATTR                
188479           ELSE                                                           
188480             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRETFG-ATTR                  
188481             MOVE NEJ TO INDATA-SW                                        
188482           END-IF                                                         
188483         END-IF                                                           
188484                                                                          
188485         IF MID-FLAUTREM NOT = ALL '+'                                    
188486           IF MID-FLAUTREM = 'Y' OR 'J' OR 'N'                            
188487             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAUTREM-ATTR               
188488           ELSE                                                           
188489             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAUTREM-ATTR                 
188490             MOVE NEJ TO INDATA-SW                                        
188491           END-IF                                                         
188492         END-IF                                                           
188493                                                                          
188494         IF MID-FLOKFAK-K NOT = ALL '+'                                   
188495           IF MID-FLOKFAK-K = 'Y' OR 'J' OR 'N'                           
188496             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLOKFAK-K-ATTR              
188497           ELSE                                                           
188498             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOKFAK-K-ATTR                
188499             MOVE NEJ TO INDATA-SW                                        
188500           END-IF                                                         
188501         END-IF                                                           
188502                                                                          
188503         IF MID-FLOKFAK-N NOT = ALL '+'                                   
188504           IF MID-FLOKFAK-N = 'Y' OR 'J' OR 'N'                           
188505             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLOKFAK-N-ATTR              
188506*EJ GODKÄNT ATT KUNNA HA BÅDE R-FAKTURA OCH N-FAKTURA                     
188507*TL 021210                                                                
188508             IF MID-FLOKFAK-N = 'Y' OR 'J'                                
188509               IF (MID-FLOKFAK-R = 'Y' OR 'J')      OR                    
188510                  (GMTA-GMT-FLOKFAK-R = 'Y' OR 'J') OR                    
188511                  (MID-FLOKFAK-G = 'Y' OR 'J')      OR                    
188512                  (GMTA-GMT-FLOKFAK-G = 'Y' OR 'J')                       
188513                                                                          
188514                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOKFAK-N-ATTR            
188515                 MOVE NEJ TO INDATA-SW                                    
188516               END-IF                                                     
188517             END-IF                                                       
188518           ELSE                                                           
188519             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOKFAK-N-ATTR                
188520             MOVE NEJ TO INDATA-SW                                        
188521           END-IF                                                         
188522         END-IF                                                           
188523                                                                          
188524         IF MID-FLOKFAK-R NOT = ALL '+'                                   
188525           IF MID-FLOKFAK-R = 'Y' OR 'J' OR 'N'                           
188526             IF (MID-FLOKFAK-R = 'Y'                                      
188527             OR  MID-FLOKFAK-R = 'J')                                     
188528             AND GMTA-GMT-TISTADAT > 0                                    
188529               MOVE GMTA-GMT-IDPARTNR TO W-WDB1-IDPARTNR                  
188530               MOVE GMTA-GMT-IDFTG    TO W-WDB1-IDFTG                     
188531               PERFORM IMS-GU-WDB101                                      
188532               IF  SEGMENT-FINNS                                          
188533                 AND (WDB1-BET-IDPARTNR(1:2) NOT NUMERIC OR               
188534****  VILKET INNEBÄR ATT PARMANUMMER SAKNAS, INTERNA KUNDER               
188535****  SOM SAKNAR PARMANR HAR T EX IDPARTNR = 'SE100    '                  
188536                 WDB1-BET-IDPROMR  = SPACE OR                             
188537                 WDB1-BET-KDVALISO = SPACE)                               
188538                  MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOKFAK-R-ATTR           
188539                  MOVE NEJ TO INDATA-SW                                   
188540               ELSE                                                       
188541                  MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLOKFAK-R-ATTR         
188542               END-IF                                                     
188543*EJ GODKÄNT ATT KUNNA HA BÅDE R-FAKTURA OCH N-FAKTURA                     
188544*TL 021210                                                                
188545               IF (MID-FLOKFAK-N = 'Y' OR 'J')    OR                      
188546                (GMTA-GMT-FLOKFAK-N = 'Y' OR 'J')                         
188547                                                                          
188548                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOKFAK-R-ATTR            
188549                 MOVE NEJ TO INDATA-SW                                    
188550               ELSE                                                       
188551                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLOKFAK-R-ATTR          
188552               END-IF                                                     
188553             ELSE                                                         
188554               MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLOKFAK-R-ATTR            
188555             END-IF                                                       
188556           ELSE                                                           
188557             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOKFAK-R-ATTR                
188558             MOVE NEJ TO INDATA-SW                                        
188559           END-IF                                                         
188560         END-IF                                                           
188561                                                                          
188562         IF MID-KDGENFAK NOT = ALL '+'                                    
188563           IF MID-KDGENFAK = 'R' OR 'N' OR 'G' OR 'K'                     
188564             PERFORM GAA-KOLLA-GENFAK                                     
188565           ELSE                                                           
188566             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDGENFAK-ATTR                 
188567             MOVE NEJ TO INDATA-SW                                        
188568           END-IF                                                         
188569         END-IF                                                           
188570     .                                                                    
188571                                                                          
188572                                                                          
188573 GAA-KOLLA-GENFAK SECTION.                                                
188574     MOVE 'GAA-KOLLA-GENFAK' TO CURRENT-SECTION                           
188575                                                                          
188576     IF MID-KDGENFAK = 'R'                                                
188577       IF (MID-FLOKFAK-R = 'Y' OR 'J')                                    
188578         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDGENFAK-ATTR                   
188579       ELSE                                                               
188580         IF GMTA-GMT-FLOKFAK-R = JA                                       
188581           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDGENFAK-ATTR                 
188582         ELSE                                                             
188583           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDGENFAK-ATTR                 
188584           MOVE NEJ TO INDATA-SW                                          
188585         END-IF                                                           
188586       END-IF                                                             
188587     ELSE                                                                 
188588       IF MID-KDGENFAK = 'N'                                              
188589         IF (MID-FLOKFAK-N = 'Y' OR 'J')                                  
188590           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDGENFAK-ATTR                 
188591         ELSE                                                             
188592           IF GMTA-GMT-FLOKFAK-N = JA                                     
188593             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDGENFAK-ATTR               
188594           ELSE                                                           
188595             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDGENFAK-ATTR                 
188596             MOVE NEJ TO INDATA-SW                                        
188597           END-IF                                                         
188598         END-IF                                                           
188599       ELSE                                                               
188600         IF MID-KDGENFAK = 'G'                                            
188601           IF (MID-FLOKFAK-G = 'Y' OR 'J')                                
188602             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDGENFAK-ATTR               
188603           ELSE                                                           
188604             IF GMTA-GMT-FLOKFAK-G = JA                                   
188605               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDGENFAK-ATTR             
188606             ELSE                                                         
188607               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDGENFAK-ATTR               
188608               MOVE NEJ TO INDATA-SW                                      
188609             END-IF                                                       
188610           END-IF                                                         
188611         ELSE                                                             
188612           IF MID-KDGENFAK = 'K'                                          
188613             IF (MID-FLOKFAK-K = 'Y' OR 'J')                              
188614               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDGENFAK-ATTR             
188615             ELSE                                                         
188616               IF GMTA-GMT-FLOKFAK-K = JA                                 
188617                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDGENFAK-ATTR           
188618               ELSE                                                       
188619                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDGENFAK-ATTR             
188620                 MOVE NEJ TO INDATA-SW                                    
188621               END-IF                                                     
188622             END-IF                                                       
188623           END-IF                                                         
188624         END-IF                                                           
188625       END-IF                                                             
188626     END-IF                                                               
188627     .                                                                    
188628 GC-KOLLA-V-DATA SECTION.                                                 
188629     MOVE 'GC-KOLLA-V-DATA ' TO CURRENT-SECTION                           
188630                                                                          
188631     IF MID-FLLDCKND NOT = ALL '+'                                        
188632        IF MID-FLLDCKND = 'Y' OR 'J' OR 'N'                               
188633           MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLLDCKND-ATTR               
188634           IF MID-FLLDCKND = 'Y'                                          
188635              MOVE JA                  TO MID-FLLDCKND                    
188636           END-IF                                                         
188637        ELSE                                                              
188638           MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLLDCKND-ATTR               
188639           MOVE NEJ                    TO INDATA-SW                       
188640        END-IF                                                            
188641     ELSE                                                                 
188642        MOVE GMTA-GMT-FLLDCKND         TO MID-FLLDCKND                    
188643     END-IF                                                               
188644                                                                          
188645     IF MID-KVDAGAR-SDC NOT = ALL '+'                                     
188646        INSPECT MID-KVDAGAR-SDC REPLACING LEADING SPACE BY ZERO           
188647        IF MID-KVDAGAR-SDC NUMERIC                                        
188648           MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVDAGAR-SDC-ATTR            
188649        ELSE                                                              
188650           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVDAGAR-SDC-ATTR            
188651           MOVE NEJ                    TO INDATA-SW                       
188652        END-IF                                                            
188653     ELSE                                                                 
188654        MOVE GMTA-GMT-KVDAGAR-SDC      TO MID-KVDAGAR-SDC                 
188655     END-IF                                                               
188656                                                                          
188657     IF MID-KVDAGAR-CDC NOT = ALL '+'                                     
188658        INSPECT MID-KVDAGAR-CDC REPLACING LEADING SPACE BY ZERO           
188659        IF MID-KVDAGAR-CDC NUMERIC                                        
188660           MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVDAGAR-CDC-ATTR            
188661        ELSE                                                              
188662           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVDAGAR-CDC-ATTR            
188663           MOVE NEJ                    TO INDATA-SW                       
188664        END-IF                                                            
188665     ELSE                                                                 
188666        MOVE GMTA-GMT-KVDAGAR-CDC      TO MID-KVDAGAR-CDC                 
188667     END-IF                                                               
188668                                                                          
188669     IF MID-FLRETUR NOT = ALL '+'                                         
188670        IF MID-FLRETUR = 'Y' OR 'J' OR 'N'                                
188671           MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLRETUR-ATTR                
188672           IF MID-FLRETUR = 'Y'                                           
188673              MOVE JA                  TO MID-FLRETUR                     
188674           END-IF                                                         
188675        ELSE                                                              
188676           MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLRETUR-ATTR                
188677           MOVE NEJ                    TO INDATA-SW                       
188678        END-IF                                                            
188679     ELSE                                                                 
188680        MOVE GMTA-GMT-FLRETUR          TO MID-FLRETUR                     
188681     END-IF                                                               
188682                                                                          
188683     IF MID-IDDC-RET72(1) NOT = ALL '+'                                   
188684        MOVE MID-IDDC-RET72(1)          TO WS-IDDC                        
188685        PERFORM GBA-KOLLA-IDDC                                            
188686        IF IDDC-OK                                                        
188687           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDDC-RET72-ATTR(1)         
188688        ELSE                                                              
188689           MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDDC-RET72-ATTR(1)         
188690           MOVE NEJ                     TO INDATA-SW                      
188691        END-IF                                                            
188692     ELSE                                                                 
188693        MOVE GMTA-GMT-IDDC-RET72(1)     TO MID-IDDC-RET72(1)              
188694     END-IF                                                               
188695                                                                          
188696     IF MID-IDDC-RET72(2) NOT = ALL '+'                                   
188697        MOVE MID-IDDC-RET72(2)          TO WS-IDDC                        
188698        PERFORM GBA-KOLLA-IDDC                                            
188699        IF IDDC-OK                                                        
188700           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDDC-RET72-ATTR(2)         
188701        ELSE                                                              
188702           MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDDC-RET72-ATTR(2)         
188703           MOVE NEJ                     TO INDATA-SW                      
188704        END-IF                                                            
188705     ELSE                                                                 
188706        MOVE GMTA-GMT-IDDC-RET72(2)     TO MID-IDDC-RET72(2)              
188707     END-IF                                                               
188708                                                                          
188709     IF MID-IDDC-RET72(3) NOT = ALL '+'                                   
188710        MOVE MID-IDDC-RET72(3)          TO WS-IDDC                        
188711        PERFORM GBA-KOLLA-IDDC                                            
188712        IF IDDC-OK                                                        
188713           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDDC-RET72-ATTR(3)         
188714        ELSE                                                              
188715           MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDDC-RET72-ATTR(3)         
188716           MOVE NEJ                     TO INDATA-SW                      
188717        END-IF                                                            
188718     ELSE                                                                 
188719        MOVE GMTA-GMT-IDDC-RET72(3)     TO MID-IDDC-RET72(3)              
188720     END-IF                                                               
188721                                                                          
188722     PERFORM                                                              
188723     VARYING INDX4 FROM 1 BY 1                                            
188724        UNTIL INDX4 > INDX4-MAX                                           
188725        INSPECT MID-KVDAGAR-RFS (INDX4)                                   
188726           REPLACING LEADING SPACE BY ZERO                                
188727        IF MID-IDDC-RFS (INDX4) = GMTA-GMT-IDDC-RFS (INDX4)               
188728           AND MID-KVDAGAR-RFS (INDX4) =                                  
188729               GMTA-GMT-KVDAGAR-RFS (INDX4)                               
188730           MOVE ALL-PLUS              TO MID-IDDC-RFS (INDX4)             
188731                                         MID-KVDAGAR-RFS   (INDX4)        
188732        END-IF                                                            
188733     END-PERFORM                                                          
188734                                                                          
188735     PERFORM                                                              
188736     VARYING INDX4 FROM 1 BY 1                                            
188737        UNTIL INDX4 > INDX4-MAX                                           
188738        IF MID-IDDC-RFS (INDX4) = ALL '+' OR SPACE                        
188739           IF MID-KVDAGAR-RFS (INDX4) = ALL '+' OR SPACE                  
188740                                                  OR ZERO                 
188741              IF MID-IDDC-RFS (INDX4) = ALL '+' AND                       
188742                 MID-KVDAGAR-RFS (INDX4) = ALL '+'                        
188743                 MOVE GMTA-GMT-IDDC-RFS (INDX4)                           
188744                                       TO MID-IDDC-RFS (INDX4)            
188745                 MOVE GMTA-GMT-KVDAGAR-RFS (INDX4)                        
188746                                       TO MID-KVDAGAR-RFS(INDX4)          
188747              END-IF                                                      
188748           ELSE                                                           
188749              MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-RFS-ATTR               
188750                                                     (INDX4)              
188751              MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVDAGAR-RFS-ATTR            
188752                                                     (INDX4)              
188753              MOVE NEJ                 TO INDATA-SW                       
188754           END-IF                                                         
188755        ELSE                                                              
188756           MOVE MID-IDDC-RFS (INDX4)   TO WS-IDDC                         
188757           PERFORM GBA-KOLLA-IDDC                                         
188758           IF IDDC-OK                                                     
188759              MOVE MFS-ALFA-FAELT-RAETT                                   
188760                                       TO MOD-IDDC-RFS-ATTR               
188761                                                     (INDX4)              
188762           ELSE                                                           
188763              MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-RFS-ATTR               
188764                                                     (INDX4)              
188765           END-IF                                                         
188766                                                                          
188767           IF MID-KVDAGAR-RFS (INDX4) NOT = ALL '+'                       
188768              IF MID-KVDAGAR-RFS (INDX4) NUMERIC AND                      
188769                 MID-KVDAGAR-RFS (INDX4) NOT ZERO                         
188770                 MOVE MFS-NUM-FAELT-RAETT                                 
188771                                       TO MOD-KVDAGAR-RFS-ATTR            
188772                                                         (INDX4)          
188773              ELSE                                                        
188774                 MOVE MFS-NUM-FAELT-FEL                                   
188775                                       TO MOD-KVDAGAR-RFS-ATTR            
188776                                                         (INDX4)          
188777                 MOVE NEJ              TO INDATA-SW                       
188778              END-IF                                                      
188779           ELSE                                                           
188780              MOVE MFS-NUM-FAELT-FEL   TO MOD-KVDAGAR-RFS-ATTR            
188781                                                         (INDX4)          
188782              MOVE NEJ                 TO INDATA-SW                       
188783           END-IF                                                         
188784        END-IF                                                            
188785     END-PERFORM                                                          
188786                                                                          
188787     IF MID-KVDAGAR-RFS-DEF NOT = ALL '+'                                 
188788        INSPECT MID-KVDAGAR-RFS-DEF                                       
188789        REPLACING LEADING SPACE BY ZERO                                   
188790        IF MID-KVDAGAR-RFS-DEF NUMERIC                                    
188791           MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVDAGAR-RFS-DEF-ATTR        
188792        ELSE                                                              
188793           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVDAGAR-RFS-DEF-ATTR        
188794           MOVE NEJ           TO INDATA-SW                                
188795        END-IF                                                            
188796     ELSE                                                                 
188797        MOVE GMTA-GMT-KVDAGAR-RFS-DEF  TO MID-KVDAGAR-RFS-DEF             
188798     END-IF                                                               
188799                                                                          
188800     IF MID-FLKVBRYT-ORDKL1 NOT = ALL '+'                                 
188801        IF MID-FLKVBRYT-ORDKL1 = 'Y' OR 'J' OR 'N'                        
188802           MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLKVBRYT-ORDKL1-ATTR        
188803           IF MID-FLKVBRYT-ORDKL1 = 'Y'                                   
188804              MOVE JA  TO MID-FLKVBRYT-ORDKL1                             
188805           END-IF                                                         
188806        ELSE                                                              
188807           MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLKVBRYT-ORDKL1-ATTR        
188808           MOVE NEJ           TO INDATA-SW                                
188809        END-IF                                                            
188810     ELSE                                                                 
188811        MOVE GMTA-GMT-FLKVBRYT-ORDKL1  TO MID-FLKVBRYT-ORDKL1             
188812     END-IF                                                               
188813                                                                          
188814     IF MID-FLKVBRYT-ORDKL2 NOT = ALL '+'                                 
188815        IF MID-FLKVBRYT-ORDKL2 = 'Y' OR 'J' OR 'N'                        
188816           MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLKVBRYT-ORDKL2-ATTR        
188817           IF MID-FLKVBRYT-ORDKL2 = 'Y'                                   
188818              MOVE JA                  TO MID-FLKVBRYT-ORDKL2             
188819           END-IF                                                         
188820        ELSE                                                              
188821           MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLKVBRYT-ORDKL2-ATTR        
188822           MOVE NEJ                    TO INDATA-SW                       
188823        END-IF                                                            
188824     ELSE                                                                 
188825        MOVE GMTA-GMT-FLKVBRYT-ORDKL2                                     
188826                                       TO MID-FLKVBRYT-ORDKL2             
188827     END-IF                                                               
188828                                                                          
188829     IF MID-FLKVBRYT-ORDKL3 NOT = ALL '+'                                 
188830        IF MID-FLKVBRYT-ORDKL3 = 'Y' OR 'J' OR 'N'                        
188831           MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLKVBRYT-ORDKL3-ATTR        
188832           IF MID-FLKVBRYT-ORDKL3 = 'Y'                                   
188833              MOVE JA                  TO MID-FLKVBRYT-ORDKL3             
188834           END-IF                                                         
188835        ELSE                                                              
188836           MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLKVBRYT-ORDKL3-ATTR        
188837           MOVE NEJ                    TO INDATA-SW                       
188838        END-IF                                                            
188839     ELSE                                                                 
188840        MOVE GMTA-GMT-FLKVBRYT-ORDKL3  TO MID-FLKVBRYT-ORDKL3             
188841     END-IF                                                               
188842                                                                          
188843     IF MID-FLKVBRYT-ORDKL4 NOT = ALL '+'                                 
188844        IF MID-FLKVBRYT-ORDKL4 = 'Y' OR 'J' OR 'N'                        
188845           MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLKVBRYT-ORDKL4-ATTR        
188846           IF MID-FLKVBRYT-ORDKL4 = 'Y'                                   
188847              MOVE JA                  TO MID-FLKVBRYT-ORDKL4             
188848           END-IF                                                         
188849        ELSE                                                              
188850           MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLKVBRYT-ORDKL4-ATTR        
188851           MOVE NEJ                    TO INDATA-SW                       
188852        END-IF                                                            
188853     ELSE                                                                 
188854        MOVE GMTA-GMT-FLKVBRYT-ORDKL4  TO MID-FLKVBRYT-ORDKL4             
188855     END-IF                                                               
188856                                                                          
188857     IF MID-FLLDCKND = 'Y' OR 'J'                                         
188858        IF MID-KVDAGAR-SDC = ZERO                                         
188859           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVDAGAR-SDC-ATTR            
188860           MOVE NEJ                    TO INDATA-SW                       
188861        ELSE                                                              
188862           MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVDAGAR-SDC-ATTR            
188863        END-IF                                                            
188864        IF MID-KVDAGAR-CDC = ZERO                                         
188865           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVDAGAR-CDC-ATTR            
188866           MOVE NEJ                    TO INDATA-SW                       
188867        ELSE                                                              
188868           MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVDAGAR-CDC-ATTR            
188869        END-IF                                                            
188870     ELSE                                                                 
188871        IF MID-KVDAGAR-SDC > ZERO                                         
188872           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVDAGAR-SDC-ATTR            
188873           MOVE NEJ                    TO INDATA-SW                       
188874        ELSE                                                              
188875           MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVDAGAR-SDC-ATTR            
188876        END-IF                                                            
188877        IF MID-KVDAGAR-CDC > ZERO                                         
188878           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVDAGAR-CDC-ATTR            
188879           MOVE NEJ                    TO INDATA-SW                       
188880        ELSE                                                              
188881           MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVDAGAR-CDC-ATTR            
188882        END-IF                                                            
188883        IF MID-KVDAGAR-RFS-DEF > ZERO                                     
188884           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVDAGAR-RFS-DEF-ATTR        
188885           MOVE NEJ                    TO INDATA-SW                       
188886        ELSE                                                              
188887           MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVDAGAR-RFS-DEF-ATTR        
188888        END-IF                                                            
188889     END-IF                                                               
188890     .                                                                    
188891                                                                          
188892                                                                          
188893 GBA-KOLLA-IDDC    SECTION.                                               
188894     MOVE 'GBA-KOLLA-IDDC  ' TO CURRENT-SECTION.                          
188895                                                                          
188896     MOVE JA      TO IDDC-SW                                              
188897     IF WS-IDDC > SPACE                                                   
188898        MOVE WS-IDDC TO W-IDDC-B6                                         
188899        PERFORM IMS-GU-WDB601                                             
188900        IF SEGMENT-SAKNAS                                                 
188901           MOVE NEJ  TO IDDC-SW                                           
188902        END-IF                                                            
188903     END-IF                                                               
188904     .                                                                    
188905                                                                          
188906 H-UPPDATERA SECTION.                                                     
188907     MOVE 'H-UPPDATERA     ' TO CURRENT-SECTION                           
188908                                                                          
188909     PERFORM IMS-GHU-WDB201                                               
188910     IF SEGMENT-FINNS                                                     
188911       IF BORTTAG-OK                                                      
188912         PERFORM IMS-DLET-WDB201                                          
188913       ELSE                                                               
188920                                                                          
189000         IF MID-IDDC-RET NOT = ALL '+'                                    
189100           MOVE MID-IDDC-RET TO GMTA-GMT-IDDC-RET                         
189200           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDC-RET-ATTR                
189210           MOVE JA TO UPD-SW                                              
189300         ELSE                                                             
189400           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-RET-ATTR                    
189500         END-IF                                                           
189510                                                                          
189520         IF MID-FLRETFG NOT = ALL '+'                                     
189530           IF MID-FLRETFG   = YES                                         
189540             MOVE 'J'         TO GMTA-GMT-FLRETFG                         
189550           ELSE                                                           
189560             MOVE MID-FLRETFG   TO GMTA-GMT-FLRETFG                       
189570             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLRETFG-ATTR               
189580           END-IF                                                         
189581           MOVE JA TO UPD-SW                                              
189590         ELSE                                                             
189591           MOVE MFS-ROER-EJ-FAELT TO MOD-FLRETFG-ATTR                     
189592         END-IF                                                           
189600                                                                          
189700         IF MID-FLAUTREM NOT = ALL '+'                                    
189800           IF MID-FLAUTREM   = YES                                        
189900             MOVE 'J'         TO GMTA-GMT-FLAUTREM                        
190000           ELSE                                                           
190100             MOVE MID-FLAUTREM  TO GMTA-GMT-FLAUTREM                      
190200             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLAUTREM-ATTR              
190300           END-IF                                                         
190310           MOVE JA TO UPD-SW                                              
190320         ELSE                                                             
190330           MOVE MFS-ROER-EJ-FAELT TO MOD-FLAUTREM-ATTR                    
190340         END-IF                                                           
190350                                                                          
190400         IF MID-FLOKFAK-G NOT = ALL '+'                                   
190500           IF MID-FLOKFAK-G = YES                                         
190600             MOVE 'J'         TO GMTA-GMT-FLOKFAK-G                       
190700           ELSE                                                           
190800             MOVE MID-FLOKFAK-G TO GMTA-GMT-FLOKFAK-G                     
190900             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLOKFAK-G-ATTR             
191000           END-IF                                                         
191010           MOVE JA TO UPD-SW                                              
191100         ELSE                                                             
191200           MOVE MFS-ROER-EJ-FAELT TO MOD-FLOKFAK-G-ATTR                   
191300         END-IF                                                           
192500                                                                          
192600         IF MID-FLOKFAK-K NOT = ALL '+'                                   
192700           IF MID-FLOKFAK-K = YES                                         
192800             MOVE 'J'         TO GMTA-GMT-FLOKFAK-K                       
192900           ELSE                                                           
193000             MOVE MID-FLOKFAK-K TO GMTA-GMT-FLOKFAK-K                     
193100             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLOKFAK-K-ATTR             
193200           END-IF                                                         
193210           MOVE JA TO UPD-SW                                              
193300         ELSE                                                             
193400           MOVE MFS-ROER-EJ-FAELT TO MOD-FLOKFAK-K-ATTR                   
193500         END-IF                                                           
193600                                                                          
193700         IF MID-FLOKFAK-N NOT = ALL '+'                                   
193800           IF MID-FLOKFAK-N = YES                                         
193900             MOVE 'J'         TO GMTA-GMT-FLOKFAK-N                       
194000           ELSE                                                           
194100             MOVE MID-FLOKFAK-N TO GMTA-GMT-FLOKFAK-N                     
194200             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLOKFAK-N-ATTR             
194300           END-IF                                                         
194310           MOVE JA TO UPD-SW                                              
194400         ELSE                                                             
194500           MOVE MFS-ROER-EJ-FAELT TO MOD-FLOKFAK-N-ATTR                   
194600         END-IF                                                           
194700                                                                          
194800         IF MID-FLOKFAK-R NOT = ALL '+'                                   
194900           IF MID-FLOKFAK-R = YES                                         
195000             MOVE 'J'         TO GMTA-GMT-FLOKFAK-R                       
195100           ELSE                                                           
195200             MOVE MID-FLOKFAK-R TO GMTA-GMT-FLOKFAK-R                     
195300             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLOKFAK-R-ATTR             
195400           END-IF                                                         
195410           MOVE JA TO UPD-SW                                              
195500         ELSE                                                             
195600           MOVE MFS-ROER-EJ-FAELT TO MOD-FLOKFAK-R-ATTR                   
195700         END-IF                                                           
195800                                                                          
195900         IF MID-KDGENFAK NOT = ALL '+'                                    
196000           MOVE MID-KDGENFAK TO GMTA-GMT-KDGENFAK                         
196100           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDGENFAK-ATTR                
196110           MOVE JA TO UPD-SW                                              
196200         ELSE                                                             
196300           MOVE MFS-ROER-EJ-FAELT TO MOD-KDGENFAK-ATTR                    
196400         END-IF                                                           
196401                                                                          
196402         IF MID-FLLDCKND NOT = ALL '+'                                    
196403           MOVE MID-FLLDCKND TO GMTA-GMT-FLLDCKND                         
196404           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLLDCKND-ATTR                
196405           MOVE JA TO UPD-SW                                              
196406         ELSE                                                             
196407           MOVE MFS-ROER-EJ-FAELT TO MOD-FLLDCKND-ATTR                    
196408         END-IF                                                           
196409                                                                          
196410         IF MID-KVDAGAR-SDC NOT = ALL '+'                                 
196411           MOVE MID-KVDAGAR-SDC TO GMTA-GMT-KVDAGAR-SDC                   
196412           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVDAGAR-SDC-ATTR             
196413           MOVE JA TO UPD-SW                                              
196414         ELSE                                                             
196415           MOVE MFS-ROER-EJ-FAELT TO MOD-KVDAGAR-SDC-ATTR                 
196416         END-IF                                                           
196417                                                                          
196418         IF MID-KVDAGAR-CDC NOT = ALL '+'                                 
196419           MOVE MID-KVDAGAR-CDC TO GMTA-GMT-KVDAGAR-CDC                   
196420           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVDAGAR-CDC-ATTR             
196421           MOVE JA TO UPD-SW                                              
196422         ELSE                                                             
196423           MOVE MFS-ROER-EJ-FAELT TO MOD-KVDAGAR-CDC-ATTR                 
196424         END-IF                                                           
196425                                                                          
196426         IF MID-FLRETUR NOT = ALL '+'                                     
196427           MOVE MID-FLRETUR TO GMTA-GMT-FLRETUR                           
196428           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLRETUR-ATTR                 
196429           MOVE JA TO UPD-SW                                              
196430         ELSE                                                             
196431           MOVE MFS-ROER-EJ-FAELT TO MOD-FLLDCKND-ATTR                    
196432         END-IF                                                           
196433                                                                          
196434         MOVE 1 TO INDX3                                                  
196435         PERFORM UNTIL INDX3 > INDX3-MAX                                  
196436            IF MID-IDDC-RET72(INDX3) NOT = ALL '+'                        
196437               MOVE MID-IDDC-RET72(INDX3) TO                              
196438                 GMTA-GMT-IDDC-RET72(INDX3)                               
196439               MOVE MFS-ADD-LYS-UPP-FAELT TO                              
196440                 MOD-IDDC-RET72-ATTR(INDX3)                               
196441               MOVE JA TO UPD-SW                                          
196442            ELSE                                                          
196443              MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-RET72-ATTR(INDX3)        
196444            END-IF                                                        
196445            ADD 1 TO INDX3                                                
196446         END-PERFORM                                                      
196447                                                                          
196448         MOVE 1 TO INDX4                                                  
196449         PERFORM UNTIL INDX4 > INDX4-MAX                                  
196450            IF MID-IDDC-RFS(INDX4) NOT = ALL '+'                          
196451               MOVE MID-IDDC-RFS(INDX4) TO                                
196452                 GMTA-GMT-IDDC-RFS(INDX4)                                 
196453               MOVE MFS-ADD-LYS-UPP-FAELT TO                              
196454                 MOD-IDDC-RFS-ATTR(INDX4)                                 
196455               MOVE JA TO UPD-SW                                          
196456            ELSE                                                          
196457              MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-RFS-ATTR(INDX4)          
196458            END-IF                                                        
196459            IF MID-KVDAGAR-RFS(INDX4) NOT = ALL '+'                       
196460               MOVE MID-KVDAGAR-RFS(INDX4) TO                             
196461                 GMTA-GMT-KVDAGAR-RFS(INDX4)                              
196462               MOVE MFS-ADD-LYS-UPP-FAELT TO                              
196463                 MOD-KVDAGAR-RFS-ATTR(INDX4)                              
196464               MOVE JA TO UPD-SW                                          
196465            ELSE                                                          
196466              MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-RFS-ATTR(INDX4)          
196467            END-IF                                                        
196468            ADD 1 TO INDX4                                                
196469         END-PERFORM                                                      
196470                                                                          
196471         IF MID-KVDAGAR-RFS-DEF NOT = ALL '+'                             
196472           MOVE MID-KVDAGAR-RFS-DEF TO GMTA-GMT-KVDAGAR-RFS-DEF           
196473           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVDAGAR-RFS-DEF-ATTR         
196474           MOVE JA TO UPD-SW                                              
196475         ELSE                                                             
196476           MOVE MFS-ROER-EJ-FAELT TO MOD-KVDAGAR-RFS-DEF-ATTR             
196477         END-IF                                                           
196478                                                                          
196479         IF MID-FLKVBRYT-ORDKL1 NOT = ALL '+'                             
196480           MOVE MID-FLKVBRYT-ORDKL1 TO GMTA-GMT-FLKVBRYT-ORDKL1           
196481           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLKVBRYT-ORDKL1-ATTR         
196482           MOVE JA TO UPD-SW                                              
196483         ELSE                                                             
196484           MOVE MFS-ROER-EJ-FAELT TO MOD-FLKVBRYT-ORDKL1-ATTR             
196485         END-IF                                                           
196486                                                                          
196487         IF MID-FLKVBRYT-ORDKL2 NOT = ALL '+'                             
196488           MOVE MID-FLKVBRYT-ORDKL2 TO GMTA-GMT-FLKVBRYT-ORDKL2           
196489           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLKVBRYT-ORDKL2-ATTR         
196490           MOVE JA TO UPD-SW                                              
196491         ELSE                                                             
196492           MOVE MFS-ROER-EJ-FAELT TO MOD-FLKVBRYT-ORDKL2-ATTR             
196493         END-IF                                                           
196494                                                                          
196495         IF MID-FLKVBRYT-ORDKL3 NOT = ALL '+'                             
196496           MOVE MID-FLKVBRYT-ORDKL3 TO GMTA-GMT-FLKVBRYT-ORDKL3           
196497           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLKVBRYT-ORDKL3-ATTR         
196498           MOVE JA TO UPD-SW                                              
196499         ELSE                                                             
196500           MOVE MFS-ROER-EJ-FAELT TO MOD-FLKVBRYT-ORDKL3-ATTR             
196501         END-IF                                                           
196502                                                                          
196503         IF MID-FLKVBRYT-ORDKL4 NOT = ALL '+'                             
196504           MOVE MID-FLKVBRYT-ORDKL4 TO GMTA-GMT-FLKVBRYT-ORDKL4           
196505           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLKVBRYT-ORDKL4-ATTR         
196506           MOVE JA TO UPD-SW                                              
196507         ELSE                                                             
196508           MOVE MFS-ROER-EJ-FAELT TO MOD-FLKVBRYT-ORDKL4-ATTR             
196509         END-IF                                                           
196510                                                                          
196520         PERFORM IMS-REPL-WDB201                                          
196600       END-IF                                                             
199200                                                                          
199300       IF MID-INPUT-U-TRANS = ALL '+' AND                                 
199310          MID-INPUT-V-TRANS = ALL '+' AND                                 
199310          WS-FLAUTREM-EJ-OK                                               
199400          MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                       
199500          CALL WMEDKONV USING MED-WMEDAREA                                
199600          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
199700          PERFORM MFS-ROER-EJ-FAELT-IN                                    
199800          PERFORM MFS-ROER-EJ-FAELT-UT                                    
199900       ELSE                                                               
200000          IF UPPDATERING-GJORD                                            
200100             MOVE INF-UPDATE-DONE TO MED-IDMFSINF                         
200200             CALL WMEDKONV USING MED-WMEDAREA                             
200300             MOVE MED-MFSINF TO MOD-TEMFSINF                              
200400             PERFORM MFS-FORM-ATTR                                        
200500             PERFORM MFS-RENSA-FAELT-IN                                   
200600* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
200700          END-IF                                                          
200800       END-IF                                                             
201001     END-IF                                                               
201101     .                                                                    
201201                                                                          
201301                                                                          
257701 MFS-RENSA-FAELT-UT SECTION.                                              
257801                                                                          
257901*    --- ALLA UTDATA-FÄLT                                                 
258001     MOVE MFS-RENSA-FAELT TO MOD-IDDC-RET                                 
258002                             MOD-FLRETFG                                  
258003                             MOD-FLAUTREM                                 
258004                             MOD-FLOKFAK-G                                
258005                             MOD-FLOKFAK-K                                
258006                             MOD-FLOKFAK-N                                
258007                             MOD-FLOKFAK-R                                
258008                             MOD-KDGENFAK                                 
258009                             MOD-FLLDCKND                                 
258010                             MOD-KVDAGAR-SDC                              
258011                             MOD-KVDAGAR-CDC                              
258012                             MOD-FLRETUR                                  
258013                             MOD-IDDC-RET72(1)                            
258014                             MOD-IDDC-RET72(2)                            
258015                             MOD-IDDC-RET72(3)                            
258016                             MOD-IDDC-RFS(1)                              
258017                             MOD-IDDC-RFS(2)                              
258018                             MOD-IDDC-RFS(3)                              
258019                             MOD-IDDC-RFS(4)                              
258020                             MOD-KVDAGAR-RFS(1)                           
258021                             MOD-KVDAGAR-RFS(2)                           
258022                             MOD-KVDAGAR-RFS(3)                           
258023                             MOD-KVDAGAR-RFS(4)                           
258024                             MOD-KVDAGAR-RFS-DEF                          
258025                             MOD-FLKVBRYT-ORDKL1                          
258026                             MOD-FLKVBRYT-ORDKL2                          
258027                             MOD-FLKVBRYT-ORDKL3                          
258028                             MOD-FLKVBRYT-ORDKL4                          
258030                             MOD-FLCOD                                    
258101                             MOD-TISTADAT-COD                             
258201                             MOD-TISTATID-COD                             
258301                             MOD-TISTODAT-COD                             
258401                             MOD-TISTOTID-COD                             
259301                             MOD-TIFAKT                                   
259401                             MOD-TISTADAT                                 
259501                             MOD-TISTODAT                                 
262201     .                                                                    
262301                                                                          
262302                                                                          
263401 MFS-RENSA-FAELT-IN SECTION.                                              
263501                                                                          
263601*    --- ALLA INDATA-FÄLT                                                 
263701     MOVE MFS-RENSA-FAELT TO MOD-IDDC-RET                                 
263901                             MOD-FLRETFG                                  
263902                             MOD-FLAUTREM                                 
264001                             MOD-FLOKFAK-G                                
264101                             MOD-FLOKFAK-K                                
264201                             MOD-FLOKFAK-N                                
264301                             MOD-FLOKFAK-R                                
264401                             MOD-KDGENFAK                                 
264402                             MOD-FLLDCKND                                 
264403                             MOD-KVDAGAR-SDC                              
264404                             MOD-KVDAGAR-CDC                              
264405                             MOD-FLRETUR                                  
264406                             MOD-IDDC-RET72(1)                            
264407                             MOD-IDDC-RET72(2)                            
264408                             MOD-IDDC-RET72(3)                            
264409                             MOD-IDDC-RFS(1)                              
264410                             MOD-IDDC-RFS(2)                              
264411                             MOD-IDDC-RFS(3)                              
264412                             MOD-IDDC-RFS(4)                              
264420                             MOD-KVDAGAR-RFS(1)                           
264421                             MOD-KVDAGAR-RFS(2)                           
264422                             MOD-KVDAGAR-RFS(3)                           
264423                             MOD-KVDAGAR-RFS(4)                           
264430                             MOD-FLKVBRYT-ORDKL1                          
264440                             MOD-FLKVBRYT-ORDKL2                          
264450                             MOD-FLKVBRYT-ORDKL3                          
264460                             MOD-FLKVBRYT-ORDKL4                          
264901                                                                          
265501     .                                                                    
265601                                                                          
265602                                                                          
266701 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
266801                                                                          
266901*    --- ALLA UTDATA-FÄLT                                                 
267001     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-RET                               
267002                               MOD-FLRETFG                                
267003                               MOD-FLAUTREM                               
267004                               MOD-FLOKFAK-G                              
267005                               MOD-FLOKFAK-K                              
267006                               MOD-FLOKFAK-N                              
267007                               MOD-FLOKFAK-R                              
267008                               MOD-KDGENFAK                               
267009                               MOD-FLLDCKND                               
267010                               MOD-KVDAGAR-SDC                            
267011                               MOD-KVDAGAR-CDC                            
267020                               MOD-FLRETUR                                
267021                               MOD-IDDC-RET72(1)                          
267022                               MOD-IDDC-RET72(2)                          
267023                               MOD-IDDC-RET72(3)                          
267024                               MOD-IDDC-RFS(1)                            
267025                               MOD-IDDC-RFS(2)                            
267026                               MOD-IDDC-RFS(3)                            
267027                               MOD-IDDC-RFS(4)                            
267028                               MOD-KVDAGAR-RFS(1)                         
267029                               MOD-KVDAGAR-RFS(2)                         
267030                               MOD-KVDAGAR-RFS(3)                         
267031                               MOD-KVDAGAR-RFS(4)                         
267032                               MOD-KVDAGAR-RFS-DEF                        
267033                               MOD-FLKVBRYT-ORDKL1                        
267034                               MOD-FLKVBRYT-ORDKL2                        
267035                               MOD-FLKVBRYT-ORDKL3                        
267036                               MOD-FLKVBRYT-ORDKL4                        
267040                               MOD-FLCOD                                  
267101                               MOD-TISTADAT-COD                           
267201                               MOD-TISTATID-COD                           
267301                               MOD-TISTODAT-COD                           
267401                               MOD-TISTOTID-COD                           
268301                               MOD-TIFAKT                                 
268401                               MOD-TISTADAT                               
268501                               MOD-TISTODAT                               
271201     .                                                                    
271301                                                                          
271302                                                                          
272401 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
272501                                                                          
272601*    --- ALLA INDATA-FÄLT                                                 
272701     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-RET                               
272901                               MOD-FLRETFG                                
272902                               MOD-FLAUTREM                               
273001                               MOD-FLOKFAK-G                              
273101                               MOD-FLOKFAK-K                              
273201                               MOD-FLOKFAK-N                              
273301                               MOD-FLOKFAK-R                              
273401                               MOD-KDGENFAK                               
273501                               MOD-FLLDCKND                               
273601                               MOD-KVDAGAR-SDC                            
273602                               MOD-KVDAGAR-CDC                            
273603                               MOD-FLRETUR                                
273604                               MOD-IDDC-RET72(1)                          
273605                               MOD-IDDC-RET72(2)                          
273606                               MOD-IDDC-RET72(3)                          
273607                               MOD-IDDC-RFS(1)                            
273608                               MOD-IDDC-RFS(2)                            
273609                               MOD-IDDC-RFS(3)                            
273610                               MOD-IDDC-RFS(4)                            
273611                               MOD-KVDAGAR-RFS(1)                         
273612                               MOD-KVDAGAR-RFS(2)                         
273613                               MOD-KVDAGAR-RFS(3)                         
273614                               MOD-KVDAGAR-RFS(4)                         
273615                               MOD-KVDAGAR-RFS-DEF                        
273620                               MOD-FLKVBRYT-ORDKL1                        
273630                               MOD-FLKVBRYT-ORDKL2                        
273640                               MOD-FLKVBRYT-ORDKL3                        
273650                               MOD-FLKVBRYT-ORDKL4                        
273901                                                                          
274501     .                                                                    
274601                                                                          
274602                                                                          
275701 MFS-FORM-ATTR SECTION.                                                   
275801                                                                          
275901*    --- ALLA INDATA-FÄLT                                                 
276001     MOVE MFS-FORMATETS-ATTR TO MOD-IDDC-RET-ATTR                         
276201                                MOD-FLRETFG-ATTR                          
276202                                MOD-FLAUTREM-ATTR                         
276301                                MOD-FLOKFAK-G-ATTR                        
276401                                MOD-FLOKFAK-K-ATTR                        
276501                                MOD-FLOKFAK-N-ATTR                        
276601                                MOD-FLOKFAK-R-ATTR                        
276701                                MOD-KDGENFAK-ATTR                         
276702                                MOD-FLLDCKND-ATTR                         
276703                                MOD-KVDAGAR-SDC-ATTR                      
276704                                MOD-KVDAGAR-CDC-ATTR                      
276705                                MOD-FLRETUR-ATTR                          
276706                                MOD-IDDC-RET72-ATTR(1)                    
276707                                MOD-IDDC-RET72-ATTR(2)                    
276708                                MOD-IDDC-RET72-ATTR(3)                    
276709                                MOD-IDDC-RFS-ATTR(1)                      
276710                                MOD-IDDC-RFS-ATTR(2)                      
276720                                MOD-IDDC-RFS-ATTR(3)                      
276730                                MOD-IDDC-RFS-ATTR(4)                      
276740                                MOD-KVDAGAR-RFS-ATTR(1)                   
276750                                MOD-KVDAGAR-RFS-ATTR(2)                   
276760                                MOD-KVDAGAR-RFS-ATTR(3)                   
276770                                MOD-KVDAGAR-RFS-ATTR(4)                   
276780                                MOD-KVDAGAR-RFS-DEF-ATTR                  
276790                                MOD-FLKVBRYT-ORDKL1-ATTR                  
276800                                MOD-FLKVBRYT-ORDKL2-ATTR                  
276900                                MOD-FLKVBRYT-ORDKL3-ATTR                  
277000                                MOD-FLKVBRYT-ORDKL4-ATTR                  
277501     .                                                                    
277601                                                                          
277602                                                                          
278401* --- IMS SEKTIONER ---                                                   
278501                                                                          
278601 IMS-GET-MSG SECTION.                                                     
278701                                                                          
278801     MOVE '  QC' TO GODK-STATUSKODER                                      
278901     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
279001     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
279101     PERFORM IMS-STATUSKONTROLL                                           
279201     .                                                                    
279301                                                                          
279302                                                                          
279401 IMS-INSERT-MSG SECTION.                                                  
279501                                                                          
279601     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
279701       MOVE 'N' TO MFS-KDHUVOMR                                           
279801     END-IF                                                               
279901     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
280001     MOVE SPACE TO GODK-STATUSKODER                                       
280101     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
280201     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
280301     PERFORM IMS-STATUSKONTROLL                                           
280401     .                                                                    
280501                                                                          
280502                                                                          
280601*IMS-GU-WDB301 SECTION.                                                   
280602*    MOVE 'IMS-GU-WDB301   ' TO CURRENT-IMS-SECTION                       
280701*                                                                         
280702*    MOVE SPACE              TO ALL-SSA                                   
280801*    STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
280901*                   '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
281001*        DELIMITED BY SIZE INTO SSA1                                      
281101*    MOVE '  GE'             TO GODK-STATUSKODER                          
281201*    CALL CBLTDLI USING GHU GMTB-PCB DLI-IO-AREA-1 SSA1                   
281301*    MOVE GMTB-STATUS-CODE   TO STATUS-WS                                 
281401*    PERFORM IMS-STATUSKONTROLL                                           
281501*    .                                                                    
281601                                                                          
281602                                                                          
281701 IMS-GHU-WDB201 SECTION.                                                  
281702     MOVE 'IMS-GHU-WDB201  ' TO CURRENT-IMS-SECTION                       
281801                                                                          
281802     MOVE SPACE              TO ALL-SSA                                   
281901     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
282001         DELIMITED BY SIZE INTO SSA1                                      
282101     MOVE '  GE'             TO GODK-STATUSKODER                          
282201     CALL CBLTDLI USING GHU GMTA-PCB DLI-IO-AREA SSA1                     
282301     MOVE GMTA-STATUS-CODE   TO STATUS-WS                                 
282401     PERFORM IMS-STATUSKONTROLL                                           
282501     .                                                                    
282601                                                                          
282602                                                                          
282701 IMS-GU-WDB101 SECTION.                                                   
282702     MOVE 'IMS-GU-WDB1011  ' TO CURRENT-IMS-SECTION                       
282801                                                                          
282802     MOVE SPACE              TO ALL-SSA                                   
282901     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
283001         DELIMITED BY SIZE INTO SSA1                                      
283101     MOVE '  GE'             TO GODK-STATUSKODER                          
283201     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-3 SSA1                    
283301     MOVE WDB1-STATUS-CODE   TO STATUS-WS                                 
283401     PERFORM IMS-STATUSKONTROLL                                           
283501     .                                                                    
283601                                                                          
283602                                                                          
283701 IMS-REPL-WDB201 SECTION.                                                 
283702     MOVE 'IMS-REPL-WDB201 ' TO CURRENT-IMS-SECTION                       
283801                                                                          
283802     MOVE SPACE              TO ALL-SSA                                   
283901     MOVE '  '               TO GODK-STATUSKODER                          
284001     CALL CBLTDLI USING REPL GMTA-PCB DLI-IO-AREA                         
284101     MOVE GMTA-STATUS-CODE   TO STATUS-WS                                 
284201     PERFORM IMS-STATUSKONTROLL                                           
284301     .                                                                    
284401                                                                          
284402                                                                          
284501 IMS-DLET-WDB201 SECTION.                                                 
284502     MOVE 'IMS-DLET-WDB201 ' TO CURRENT-IMS-SECTION                       
284601                                                                          
284602     MOVE SPACE              TO ALL-SSA                                   
284701     MOVE '  '               TO GODK-STATUSKODER                          
284801     CALL CBLTDLI USING DLET GMTA-PCB DLI-IO-AREA                         
284901     MOVE GMTA-STATUS-CODE   TO STATUS-WS                                 
285001     PERFORM IMS-STATUSKONTROLL                                           
285101     .                                                                    
285201                                                                          
285202                                                                          
285301 IMS-GU-WDB601    SECTION.                                                
285302     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
285303                                                                          
285304     MOVE SPACE              TO ALL-SSA                                   
285401     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
285501         DELIMITED BY SIZE INTO SSA1                                      
285601     MOVE '  GE'             TO GODK-STATUSKODER                          
285701     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
285801     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
285901     PERFORM IMS-STATUSKONTROLL                                           
286001     IF SEGMENT-SAKNAS                                                    
286101         MOVE SPACE TO DCS-KDDC                                           
286201     END-IF                                                               
286301     .                                                                    
289801                                                                          
289802                                                                          
289901 IMS-STATUSKONTROLL SECTION.                                              
290001                                                                          
290101     SET STATUS-IX TO 1                                                   
290201     SEARCH GODK-STATUS                                                   
290301       AT END                                                             
290401         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
290501         DELIMITED BY SIZE INTO FELTEXT                                   
290601         CALL FELLOG                                                      
290701       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
290801         CONTINUE                                                         
290901     END-SEARCH                                                           
291001     .                                                                    
