000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4051500.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   OKT 90.                                                  
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        MÖJLIGHETER: PROGRAMMET VISAR VILKA ORDER SOM ARBETAS            
001200*                     MED ELLER BÖR VARA I ARBETE FÖR TILLFÄLLET,         
001300*                     MEN TAR INTE HÄNSYN TILL DIREKTLEV RADER.           
001400*                                                                         
001500*                     KUNDNR = 0  OCH/ELLER  C-LAGER = 0                  
001600*                     BETYDER ATT PROGRAMMET VISAR ALLA KUNDER/           
001700*                     ETT DISTR RESP ALLA C-LAGER.                        
001800*                                                                         
001900*                     DE STATUS PGM-ET VISAR ÄR: U*, L, L*, P,            
002000*                     P*, F. ASTERISK BETYDER ATT ALLA KOLLIN             
002100*                     OCH RADER INTE HAR SAMMA STATUS, DVS                
002200*                     PGM-ET VISAR LÄGSTA STATUS FÖR RESP KOLLI.          
002300*                                                                         
002400*                     DE ORDER DÄR HELA ORDERN HAR STATUS                 
002500*                     R, U, ELLER FL VISAS EJ.                            
002600*                                                                         
002700*                     PROGRAMMET LÄSER: WDE4                              
002800*                                       WDE6                              
002900*                                       WDB6                              
003000*                                                                         
003100*    INDATA.                                                              
003200*        TRANSAKTION: W4T515                                              
003300*        MID:         W4I51501                                            
003400*                                                                         
003500*    UTDATA.                                                              
003600*        MOD:         W4O51501                                            
003700                                                                          
003800     SKIP3                                                                
003900 ENVIRONMENT DIVISION.                                                    
004000 DATA DIVISION.                                                           
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                   PIC X(8)    VALUE 'W4051500'.                
004600                                                                          
004700 77  JA                      PIC X       VALUE 'J'.                       
004800 77  NEJ                     PIC X       VALUE 'N'.                       
004900 77  ALLA                    PIC X       VALUE 'A'.                       
005000                                                                          
005100 77  SPRAK-IX                PIC S9(9)   VALUE +0   COMP SYNC.            
005200 77  RAD-INDX                PIC S9(9)   VALUE +0   COMP SYNC.            
005300 77  MAX-TABRADER            PIC S9(9)   VALUE +14  COMP SYNC.            
005400       EJECT                                                              
005500                                                                          
005600 77  WS-IDDISTR              PIC X(4)    VALUE SPACE.                     
005700 77  WS-IDKUNDNR             PIC X(6)    VALUE SPACE.                     
005800                                                                          
005900     EJECT                                                                
006000 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
006100   88  EGEN-MID                          VALUE '4515'.                    
006200   88  GODK-MID                          VALUE '4511' '4512'              
006300                                               '4513' '4514'              
006400                                               '4515' '4516'              
006500                                               '4517' '4518'              
006600                                               '4519'.                    
006700                                                                          
006800 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
006900   88  NYCKLAR-OK                        VALUE 'J'.                       
007000   88  NYCKLAR-FEL                       VALUE 'N'.                       
007100                                                                          
007200 77  ALLT-SW                 PIC X       VALUE 'J'.                       
007300   88  ALLT-OK                           VALUE 'J'.                       
007400   88  ALLT-FEL                          VALUE 'N'.                       
007500                                                                          
007600 77  KUND-SW                 PIC X       VALUE 'A'.                       
007700   88  EN-KUND                           VALUE 'J'.                       
007800   88  ALLA-KUNDER                       VALUE 'A'.                       
007900                                                                          
008000 77  DC-SW                   PIC X       VALUE 'J'.                       
008100   88  EN-DC                             VALUE 'J'.                       
008200   88  ALLA-DC                           VALUE 'A'.                       
008300                                                                          
008400 77  IDDC-SW                 PIC X       VALUE 'J'.                       
008500   88  IDDC-RAETT                        VALUE 'J'.                       
008600   88  IDDC-FEL                          VALUE 'N'.                       
008700     EJECT                                                                
008800 01  FELM-CODES.                                                          
008900   03  FILLER                PIC X(16)   VALUE 'FELM AREA'.               
009000   03  FELM-DISTR-KUND-SAKNAS                                             
009100                             PIC X(3)    VALUE '040'.                     
009200   03  FELM-SISTA-SIDAN-REDAN-VISAD                                       
009300                             PIC X(3)    VALUE '115'.                     
009400   03  FELM-URVAL-SAKNAS     PIC X(3)    VALUE '005'.                     
009500                                                                          
009600 01  MESSAGE-CODES.                                                       
009700   03  FILLER                PIC X(16)   VALUE 'INFO AREA'.               
009800   03  INF-FOERSTA-SIDAN     PIC X(3)    VALUE '006'.                     
009900   03  INF-MER-INFO-FINNS    PIC X(3)    VALUE '105'.                     
009910   03  INF-NO-MORE-F6        PIC X(3)    VALUE '368'.                     
010000   03  INF-FEL-NYCKEL        PIC X(3)    VALUE '401'.                     
010100   03  INF-LAST-PAGE         PIC X(3)    VALUE '106'.                     
010200                                                                          
010300 01  SPAR-AREA.                                                           
010400   03  SPAR-IDKUNDNR         PIC S9(7)   VALUE ZERO COMP-3.               
010500   03  SPAR-IDORDNR5         PIC 9(5)    VALUE ZERO.                      
010600   03  SPAR-IDDC             PIC X(02)   VALUE SPACE.                     
010700   03  SPAR-KDFRAKT          PIC S9(3)   VALUE ZERO COMP-3.               
010800   03  SPAR-KDORDKL          PIC S9      VALUE ZERO COMP-3.               
010900   03  SPAR-KDORDSTA         PIC X(2)    VALUE SPACE.                     
011000   03  SPAR-KVKOLPAC         PIC S9(5)   VALUE ZERO COMP-3.               
011100   03  SPAR-KVKOLLI-FAKT     PIC S9(5)   VALUE ZERO COMP-3.               
011200   03  SPAR-KVKOLLI-LAST     PIC S9(5)   VALUE ZERO COMP-3.               
011300   03  SPAR-VKORDBTO         PIC S9(6)V9(1)                               
011400                                         VALUE ZERO COMP-3.               
011500   03  SPAR-VLORDBTO         PIC S9(4)V9(3)                               
011600                                         VALUE ZERO COMP-3.               
011700   03  SPAR-IDORDER          PIC S9(7)   VALUE ZERO COMP-3.               
011800   03  SPAR-IDPRODNR         PIC S9(7)   VALUE ZERO COMP-3.               
011900   03  SPAR-IDPLKLST         PIC S9(3)   VALUE ZERO COMP-3.               
012000 EJECT                                                                    
012100 01  TEST-IDDISTR             PIC S9(5) COMP-3.                           
012200     EJECT                                                                
012300 01  GENERELLA-SUBPROGRAM.                                                
012400   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
012500   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
012600   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
012700   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013000*01 -COPY WMSGINIT                                                        
013100     EJECT                                                                
013200*   -COPY WMEDAREA                                                        
013300     EJECT                                                                
013400*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
013500   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
013600     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
013700                                                                          
013800 01  BILD-HOPP-AREOR.                                                     
013900                                                                          
014000   03    W-BILD               PIC X(4)    VALUE SPACE.                    
014100   03    W-HOPP-IDTRANS.                                                  
014200     05  FILLER               PIC X(1)    VALUE 'W'.                      
014300     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
014400     05  FILLER               PIC X(1)    VALUE 'T'.                      
014500     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
014600     05  FILLER               PIC X(2)    VALUE SPACE.                    
014700                                                                          
014800                                                                          
014900   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
015000   03      P-TO-P-SW.                                                     
015100                                                                          
015200     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
015300     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
015400     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
015500     05  P-TO-P-KDTRANS          PIC X(8).                                
015600     05  P-TO-P-IDTRANS          PIC X(4).                                
015700     05  P-TO-P-KDMFSFOR         PIC X(1).                                
015800     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
015900******************************************************************        
016000*                                                                         
016100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
016200*                                                                         
016300 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
016400     SKIP3                                                                
016500*01  MID -COPY W4I51501                                                   
016600     EJECT                                                                
016700*01  -COPY WMSGAREA                                                       
016800     EJECT                                                                
016900*  03  MOD -COPY W4O51501 -RED MSG-AREA.                                  
017000     EJECT                                                                
017100*01  -COPY WMFSAREA                                                       
017200     EJECT                                                                
017300******************************************************************        
017400*                                                                         
017500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017600*                                                                         
017700 01  IMS-WS.                                                              
017800   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
017900     SKIP3                                                                
018000*                        **** STATUS-KOD FRÅN IMS                         
018100   03  STATUS-WS             PIC XX.                                      
018200     88  STATUS-OK                       VALUE '  '.                      
018300     88  SEGMENT-FINNS                   VALUE '  '.                      
018400     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
018500     88  BASEN-SLUT                      VALUE 'GB'.                      
018600     88  TRANSKOD-FEL                    VALUE 'A1'.                      
018700     88  SECURITY-FEL                    VALUE 'A4'.                      
018800     SKIP3                                                                
018900   03  GODK-STATUSKODER.                                                  
019000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019100     SKIP3                                                                
019200                                                                          
019300 01  SAVE-AREA.                                                           
019400   03  SAVE-IDTRANS              PIC X(4)    VALUE SPACE.                 
019500   03  PGNO                      PIC 9(2).                                
019510   03  FIRST-SW                  PIC X       VALUE 'J'.                   
019600   03  SAVE-AREA-PREV OCCURS 20 TIMES.                                    
019700         05  SAVE-IDKUNDNR-PREV   PIC S9(7)   VALUE ZERO COMP-3.          
019800         05  SAVE-IDKUNDRF-PREV   PIC X(10)   VALUE SPACE.                
019900         05  SAVE-IDPRODNR-PREV   PIC 9(7)    VALUE ZERO.                 
020000         05  SAVE-IDPLKLST-PREV   PIC X(3)    VALUE SPACE.                
020100   03  SAVE-AREA-NEXT.                                                    
020200         05  SAVE-IDKUNDNR-NEXT   PIC S9(7)   VALUE ZERO COMP-3.          
020300         05  SAVE-IDKUNDRF-NEXT   PIC X(10)   VALUE SPACE.                
020400         05  SAVE-IDPRODNR-NEXT   PIC 9(7)    VALUE ZERO.                 
020500         05  SAVE-IDPLKLST-NEXT   PIC X(3)    VALUE SPACE.                
020600                                                                          
020700 01  NYCKLAR-TILL-DLI.                                                    
020800                                                                          
020900*-----------------WDE4A1 SEKUNDÄRT INDEX                                  
021000   03  W-WDE4ASEQ-X.                                                      
021100     05  W-SEQA-IDDISTR        PIC S9(5)   VALUE ZERO  COMP-3.            
021200     05  W-SEQA-IDKUNDNR       PIC S9(7)   VALUE ZERO  COMP-3.            
021300     05  W-SEQA-IDKUNDRF       PIC X(10)   VALUE LOW-VALUE.               
021400                                                                          
021500   03  W-WDE4ASEQ-MIN-X.                                                  
021600     05  W-SEQA-IDDISTR-MIN    PIC S9(5)   VALUE ZERO  COMP-3.            
021700     05  W-SEQA-IDKUNDNR-MIN   PIC S9(7)   VALUE ZERO  COMP-3.            
021800     05  W-SEQA-IDKUNDRF-MIN   PIC X(10)   VALUE LOW-VALUE.               
021900                                                                          
022000   03  W-WDE4ASEQ-MAX-X.                                                  
022100     05  W-SEQA-IDDISTR-MAX    PIC S9(5)   VALUE ZERO  COMP-3.            
022200     05  W-SEQA-IDKUNDNR-MAX   PIC S9(7)   VALUE ZERO  COMP-3.            
022300     05  W-SEQA-IDKUNDRF-MAX   PIC X(10)   VALUE HIGH-VALUE.              
022400                                                                          
022500*-----------------WDE601                                                  
022600   03  W-IDPRODNR-X.                                                      
022700     05  W-VORD-IDPRODNR       PIC S9(7)   VALUE ZERO  COMP-3.            
022800                                                                          
022900*-----------------WDB601                                                  
023000   03  W-IDDC-B6-X.                                                       
023100       05 W-IDDC-B6                  PIC X(2).                            
023200     SKIP2                                                                
023300 01    SSA1                  PIC X(96).                                   
023400     EJECT                                                                
023500*                            IMS FUNKTIONSKODER                           
023600*01    -COPY W0003                                                        
023700     EJECT                                                                
023800*                            DLI INPUT-OUTPUT AREA                        
023900 01  DLI-IO-AREA.                                                         
024000   03  IO-AREA               PIC X(300)  VALUE SPACE.                     
024100                                                                          
024200     SKIP2                                                                
024300*  03               -COPY WDE401             -RED IO-AREA.                
024400     EJECT                                                                
024500*  03               -COPY WDE601             -RED IO-AREA.                
024600                                                                          
024700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
024800 01   DLI-IO-AREA-B601.                                                   
024900*     03  -COPY WDB601                                                    
025000     EJECT                                                                
025100 LINKAGE SECTION.                                                         
025200*01  -COPY W0009     -PRE MSG-                                            
025300     EJECT                                                                
025400*01  -COPY W0009     -PRE ALT-                                            
025500     EJECT                                                                
025600*01  -COPY W0008     -PRE USEA-                                           
025700     05  FILLER              PIC X.                                       
025800     EJECT                                                                
025900*01  -COPY W0008     -PRE WDE4-                                           
026000     05  FILLER              PIC X.                                       
026100     EJECT                                                                
026200*01  -COPY W0008     -PRE WDE6-                                           
026300     05  FILLER              PIC X.                                       
026400                                                                          
026500*01  -COPY W0008     -PRE WDB6-                                           
026600     05  FILLER              PIC X.                                       
026700                                                                          
026800     EJECT                                                                
026900 PROCEDURE DIVISION USING  MSG-PCB   ALT-PCB USEA-PCB                     
027000                           WDE4-PCB  WDE6-PCB WDB6-PCB.                   
027100     ENTRY 'DLITCBL' USING MSG-PCB   ALT-PCB USEA-PCB                     
027200                           WDE4-PCB  WDE6-PCB WDB6-PCB.                   
027300                                                                          
027400     PERFORM IMS-GET-MSG                                                  
027500     IF SEGMENT-FINNS                                                     
027600       PERFORM A-INIT                                                     
027700       PERFORM B-KOLLA-NYCKLAR                                            
027800       IF NYCKLAR-OK                                                      
027900                                                                          
028000         IF MFS-FIRST                                                     
028100            PERFORM C-FOERSTA-SIDAN                                       
028200         ELSE                                                             
028300           IF MFS-NEXT                                                    
028400             PERFORM D-NAESTA-SIDAN                                       
028500           ELSE                                                           
028600             IF MFS-PREVIOUS                                              
028700               PERFORM I-PREV-SIDA                                        
028800             ELSE                                                         
028900               PERFORM E-SAMMA-SIDA                                       
029000             END-IF                                                       
029100           END-IF                                                         
029200         END-IF                                                           
029300                                                                          
029400         IF ALLT-OK                                                       
029500            IF STARTA-ANNAN-BILD                                          
029600              CONTINUE                                                    
029700            ELSE                                                          
029800              PERFORM F-LAES-VISA-INFO                                    
029900            END-IF                                                        
030000         END-IF                                                           
030100                                                                          
030200       END-IF                                                             
030300                                                                          
030400       IF STARTA-ANNAN-BILD                                               
030500          CONTINUE                                                        
030600       ELSE                                                               
030700          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O51501 + 4                   
030800          PERFORM IMS-INSERT-MSG                                          
030900      END-IF                                                              
031000     END-IF                                                               
031100                                                                          
031200     MOVE ZERO TO RETURN-CODE                                             
031300     GOBACK                                                               
031400     .                                                                    
031500     EJECT                                                                
031600 A-INIT SECTION.                                                          
031700                                                                          
031800     IF MSG-DUBBLA-TRANSKODER                                             
031900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I51501                 
032000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
032100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
032200     ELSE                                                                 
032300       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I51501                   
032400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
032500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032600     END-IF                                                               
032700                                                                          
032800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
032900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
033000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
033100                                                                          
033200     MOVE LOW-VALUE TO MSG-AREA                                           
033300     MOVE 'W4O515N1' TO MFS-IDMOD                                         
033400     MOVE '4515' TO MOD-IDTRANS                                           
033500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
033600                                                                          
033700     IF NOT EGEN-MID                                                      
033800       MOVE SPACE TO MFS-KDTRTYP                                          
033900       MOVE '7' TO MFS-IDPFK                                              
034000     END-IF                                                               
034100                                                                          
034200     .                                                                    
034300     EJECT                                                                
034400 B-KOLLA-NYCKLAR SECTION.                                                 
034500                                                                          
034600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
034700     MOVE '001'             TO MSGI-KDCALL                                
034800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
034900     MOVE '4515'            TO MSGI-IDTRANS                               
035000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
035100     IF EGEN-MID                                                          
035200        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
035300        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
035400     END-IF                                                               
035500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
035600     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
035700     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
035800                                                                          
035900     MOVE JA TO NYCKLAR-SW                                                
036000                                                                          
036100     PERFORM BA-KOLLA-DISTRIKT                                            
036200     PERFORM BB-KOLLA-KUNDNR                                              
036300     PERFORM BC-KOLLA-DC                                                  
036400     PERFORM BD-BEHANDLA-KL-FK-STATUS                                     
036500                                                                          
036600                                                                          
036700     IF NYCKLAR-FEL                                                       
036800       MOVE INF-FEL-NYCKEL TO MED-IDMFSFEL                                
036900       CALL WMEDKONV USING MED-WMEDAREA                                   
037000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
037100       PERFORM MFS-RENSA-FAELT-UT                                         
037200                                                                          
037300       IF NOT GODK-MID                                                    
037400         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                           
037500                                 MOD-IDKUNDNR-UT                          
037600                                 MOD-IDDC-UT                              
037700                                 MOD-KDORDKL-UT                           
037800                                 MOD-KDFRAKT-UT                           
037900                                 MOD-KDORDSTA-UT                          
038000       END-IF                                                             
038100     END-IF                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 BA-KOLLA-DISTRIKT SECTION.                                               
038500                                                                          
038600     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
038700                                                                          
038800     IF MID-IDDISTR-IN     NOT = ALL '+'                                  
038900       MOVE '7'            TO MFS-IDPFK                                   
039000       MOVE SPACE          TO MFS-KDTRTYP                                 
039100     END-IF                                                               
039200                                                                          
039300     MOVE MSGI-IDDISTR     TO MOD-IDDISTR-UT                              
039400                                                                          
039500     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
039600                                                                          
039700     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
039800       MOVE MSGI-IDDISTR TO W-SEQA-IDDISTR-MIN                            
039900                            W-SEQA-IDDISTR-MAX                            
040000                            WS-IDDISTR                                    
040100                            TEST-IDDISTR                                  
040200     ELSE                                                                 
040300       MOVE NEJ        TO NYCKLAR-SW                                      
040400     END-IF                                                               
040500     .                                                                    
040600     EJECT                                                                
040700 BB-KOLLA-KUNDNR SECTION.                                                 
040800                                                                          
040900     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
041000                                                                          
041100     IF MID-IDKUNDNR-IN     NOT  = ALL '+'                                
041200       MOVE '7'           TO MFS-IDPFK                                    
041300       MOVE SPACE         TO MFS-KDTRTYP                                  
041400     END-IF                                                               
041500                                                                          
041600     MOVE MSGI-IDKUNDNR   TO MOD-IDKUNDNR-UT                              
041700     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
041800                                                                          
041900     IF MOD-IDKUNDNR-UT = SPACE                                           
042000       MOVE '     0' TO MOD-IDKUNDNR-UT                                   
042100     END-IF                                                               
042200                                                                          
042300                                                                          
042400     IF MSGI-IDKUNDNR = ZERO                                              
042500       MOVE JA TO KUND-SW                                                 
042600     ELSE                                                                 
042700       MOVE JA TO KUND-SW                                                 
042800     END-IF                                                               
042900                                                                          
043000     IF MSGI-IDKUNDNR NUMERIC                                             
043100       IF EN-KUND                                                         
043200         MOVE MSGI-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                        
043300                               W-SEQA-IDKUNDNR-MAX                        
043400       ELSE                                                               
043500         MOVE ZERO        TO W-SEQA-IDKUNDNR-MIN                          
043600         MOVE +9999999    TO W-SEQA-IDKUNDNR-MAX                          
043700       END-IF                                                             
043800                                                                          
043900     ELSE                                                                 
044000       MOVE NEJ           TO NYCKLAR-SW                                   
044100     END-IF                                                               
044200     .                                                                    
044300     EJECT                                                                
044400 BC-KOLLA-DC SECTION.                                                     
044500                                                                          
044600     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
044700                                                                          
044800     IF MID-IDDC-IN = ALL '+'                                             
044900       MOVE MID-IDDC-UT     TO W-IDDC-B6                                  
045000     ELSE                                                                 
045100       MOVE MID-IDDC-IN     TO W-IDDC-B6                                  
045200       MOVE '7'             TO MFS-IDPFK                                  
045300       MOVE SPACE           TO MFS-KDTRTYP                                
045400     END-IF                                                               
045500     PERFORM IMS-GU-WDB601                                                
045600                                                                          
045700     IF DCS-KDDC = SPACE OR DCS-CDC-TR                                    
045800        MOVE MSGI-IDDC       TO W-IDDC-B6                                 
045900        PERFORM IMS-GU-WDB601                                             
046000     END-IF                                                               
046100                                                                          
046200     MOVE W-IDDC-B6         TO MOD-IDDC-UT                                
046300     .                                                                    
046400     EJECT                                                                
046500 BD-BEHANDLA-KL-FK-STATUS SECTION.                                        
046600                                                                          
046700     MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-IN                               
046800                             MOD-KDFRAKT-IN                               
046900                             MOD-KDORDSTA-IN                              
047000                                                                          
047100     IF MID-KDORDKL-IN = ALL '+'                                          
047200       MOVE MID-KDORDKL-UT TO MOD-KDORDKL-UT                              
047300     ELSE                                                                 
047400       MOVE MID-KDORDKL-IN TO MOD-KDORDKL-UT                              
047500     END-IF                                                               
047600                                                                          
047700     MOVE MSGI-KDFRAKT     TO MOD-KDFRAKT-UT                              
047800                                                                          
047900     IF MID-KDORDSTA-IN = ALL '+'                                         
048000       MOVE MID-KDORDSTA-UT TO MOD-KDORDSTA-UT                            
048100     ELSE                                                                 
048200       MOVE MID-KDORDSTA-IN TO MOD-KDORDSTA-UT                            
048300     END-IF                                                               
048400     .                                                                    
048500     EJECT                                                                
048600 C-FOERSTA-SIDAN SECTION.                                                 
048700                                                                          
048800     INITIALIZE SAVE-AREA                                                 
048900     MOVE 1          TO PGNO                                              
048910     MOVE JA         TO FIRST-SW                                          
049000                                                                          
049100     MOVE LOW-VALUE  TO W-SEQA-IDKUNDRF-MIN                               
049200     MOVE HIGH-VALUE TO W-SEQA-IDKUNDRF-MAX                               
049300                                                                          
049400     PERFORM MFS-RENSA-FAELT-IN                                           
049500                                                                          
049600     MOVE INF-FOERSTA-SIDAN TO MED-IDMFSFEL                               
049700     CALL WMEDKONV USING MED-WMEDAREA                                     
049800     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
049900     .                                                                    
050000     EJECT                                                                
050100 D-NAESTA-SIDAN SECTION.                                                  
050200                                                                          
050300     IF SAVE-IDTRANS = '4515'                                             
050400                                                                          
050500       IF (SAVE-IDKUNDNR-NEXT > ZERO   OR                                 
050600           SAVE-IDKUNDRF-NEXT > SPACES OR                                 
050700           SAVE-IDPRODNR-NEXT > ZERO   AND                                
050800           SAVE-IDPLKLST-NEXT > SPACES )                                  
050900                                                                          
051000         MOVE WS-IDDISTR         TO W-SEQA-IDDISTR                        
051100         MOVE SAVE-IDKUNDNR-NEXT TO W-SEQA-IDKUNDNR                       
051200         MOVE SAVE-IDKUNDRF-NEXT TO W-SEQA-IDKUNDRF                       
051300                                                                          
051400         IF (SAVE-IDKUNDNR-PREV(PGNO) NOT = SAVE-IDKUNDNR-NEXT OR         
051500             SAVE-IDKUNDRF-PREV(PGNO) NOT = SAVE-IDKUNDRF-NEXT OR         
051600             SAVE-IDPRODNR-PREV(PGNO) NOT = SAVE-IDPRODNR-NEXT OR         
051700             SAVE-IDPLKLST-PREV(PGNO) NOT = SAVE-IDPLKLST-NEXT)           
051800           IF PGNO = 20                                                   
051900             PERFORM VARYING PGNO FROM 1 BY 1                             
052000             UNTIL PGNO = 20                                              
052100             MOVE SAVE-IDKUNDNR-PREV(PGNO + 1) TO                         
052200                                      SAVE-IDKUNDNR-PREV(PGNO)            
052300             MOVE SAVE-IDKUNDRF-PREV(PGNO + 1) TO                         
052400                                      SAVE-IDKUNDRF-PREV(PGNO)            
052500             MOVE SAVE-IDPRODNR-PREV(PGNO + 1) TO                         
052600                                      SAVE-IDPRODNR-PREV(PGNO)            
052700             MOVE SAVE-IDPLKLST-PREV(PGNO + 1) TO                         
052800                                      SAVE-IDPLKLST-PREV(PGNO)            
052900             END-PERFORM                                                  
052910             MOVE NEJ            TO FIRST-SW                              
053000           ELSE                                                           
053100              ADD 1 TO PGNO                                               
053200           END-IF                                                         
053300         END-IF                                                           
053400         MOVE JA TO ALLT-SW                                               
053500       ELSE                                                               
053600         PERFORM MFS-RENSA-FAELT-UT                                       
053700                                                                          
053800         MOVE FELM-SISTA-SIDAN-REDAN-VISAD TO MED-IDMFSFEL                
053900         CALL WMEDKONV USING MED-WMEDAREA                                 
054000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
054100         PERFORM MFS-ROER-EJ-BILD                                         
054200                                                                          
054300         MOVE NEJ TO ALLT-SW                                              
054400       END-IF                                                             
054500     ELSE                                                                 
054600       MOVE 01 TO PGNO                                                    
054700     END-IF                                                               
054800     .                                                                    
054900     EJECT                                                                
055000 I-PREV-SIDA SECTION.                                                     
055100                                                                          
055200     IF SAVE-IDTRANS = '4515'                                             
055300       IF PGNO > 1                                                        
055400         COMPUTE PGNO = PGNO - 1                                          
055500         MOVE WS-IDDISTR               TO W-SEQA-IDDISTR                  
055600         MOVE SAVE-IDKUNDNR-PREV(PGNO) TO W-SEQA-IDKUNDNR                 
055700         MOVE SAVE-IDKUNDRF-PREV(PGNO) TO W-SEQA-IDKUNDRF                 
055800       ELSE                                                               
055900          MOVE WS-IDDISTR              TO W-SEQA-IDDISTR                  
056000          MOVE SAVE-IDKUNDNR-PREV(1)   TO W-SEQA-IDKUNDNR                 
056100          MOVE SAVE-IDKUNDRF-PREV(1)   TO W-SEQA-IDKUNDRF                 
056110         IF PGNO = 1                                                      
056120           IF FIRST-SW = JA                                               
056130             MOVE INF-FOERSTA-SIDAN    TO MED-IDMFSINF                    
056140             CALL WMEDKONV          USING MED-WMEDAREA                    
056150             MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                    
056170           ELSE                                                           
056180             MOVE INF-NO-MORE-F6       TO MED-IDMFSFEL                    
056190             CALL WMEDKONV          USING MED-WMEDAREA                    
056191             MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                    
056193           END-IF                                                         
056194         END-IF                                                           
056500       END-IF                                                             
056600     END-IF                                                               
056700     MOVE JA TO ALLT-SW                                                   
056800     .                                                                    
056900     EJECT                                                                
057000 E-SAMMA-SIDA SECTION.                                                    
057100                                                                          
057200     IF SAVE-IDTRANS = '4515'                                             
057300       MOVE WS-IDDISTR               TO W-SEQA-IDDISTR                    
057400       MOVE SAVE-IDKUNDNR-PREV(PGNO) TO W-SEQA-IDKUNDNR                   
057500       MOVE SAVE-IDKUNDRF-PREV(PGNO) TO W-SEQA-IDKUNDRF                   
057600     ELSE                                                                 
057710       INITIALIZE SAVE-AREA                                               
057720       MOVE 1                   TO PGNO                                   
057730       MOVE JA                  TO FIRST-SW                               
057800     END-IF                                                               
057900                                                                          
058000     IF EGEN-MID                                                          
058100       MOVE +1                    TO RAD-INDX                             
058200       PERFORM UNTIL RAD-INDX    >  MAX-TABRADER                          
058300          IF MID-IDTRANS(RAD-INDX)   =  ALL '+'                           
058400             CONTINUE                                                     
058500          ELSE                                                            
058600             IF MID-IDTRANS(RAD-INDX) NUMERIC                             
058700                PERFORM EA-STARTA-ANNAN-BILD                              
058800                MOVE JA           TO SW-STARTA-ANNAN-BILD                 
058900                MOVE MAX-TABRADER TO RAD-INDX                             
059000             END-IF                                                       
059100          END-IF                                                          
059200          ADD +1                  TO RAD-INDX                             
059300       END-PERFORM                                                        
059400     END-IF                                                               
059500     .                                                                    
059600     EJECT                                                                
059700 EA-STARTA-ANNAN-BILD  SECTION.                                           
059800                                                                          
059900     INSPECT MID-IDKUNDNR(RAD-INDX)                                       
060000                                  REPLACING LEADING SPACE BY ZERO         
060100     MOVE MID-IDKUNDNR(RAD-INDX) TO MSGI-IDKUNDNR                         
060200     INSPECT MID-IDORDNR7(RAD-INDX)                                       
060300                                REPLACING LEADING SPACE BY ZERO           
060400     MOVE MID-IDORDNR7(RAD-INDX) TO MSGI-IDKUNDRF(1:7)                    
060500     MOVE '001'                  TO MSGI-KDCALL                           
060600     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
060700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
060800                                                                          
060900     MOVE LOW-VALUE              TO P-TO-P-KDZ1                           
061000     MOVE LOW-VALUE              TO P-TO-P-KDZ2                           
061100     MOVE MID-IDTRANS(RAD-INDX) (1:1)  TO W-HOPP-IDTRANS-2                
061200     MOVE MID-IDTRANS(RAD-INDX) (2:3)  TO W-HOPP-IDTRANS-4-6              
061300     MOVE W-HOPP-IDTRANS         TO P-TO-P-KDTRANS                        
061400     MOVE '4515'                 TO P-TO-P-IDTRANS                        
061500     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
061600                                                                          
061700     PERFORM S01-INSERT-ALTMSG                                            
061800     .                                                                    
061900     EJECT                                                                
062000 F-LAES-VISA-INFO SECTION.                                                
062100                                                                          
062200     IF MFS-ENTER OR MFS-NEXT                                             
062300       PERFORM IMS-GU-WDE401-DISTR                                        
062400     ELSE                                                                 
062500       IF MFS-FIRST                                                       
062600         PERFORM IMS-GN-WDE401-DISTR                                      
062700       END-IF                                                             
062800     END-IF                                                               
062900                                                                          
063000     IF SEGMENT-FINNS                                                     
063100                                                                          
063200       PERFORM FA-LAES-BEHANDLA-RADINFO                                   
063300                                                                          
063400       IF RAD-INDX = +1                                                   
063500         MOVE FELM-URVAL-SAKNAS TO MED-IDMFSFEL                           
063600         CALL WMEDKONV USING MED-WMEDAREA                                 
063700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
063800       END-IF                                                             
063900                                                                          
064000     ELSE                                                                 
064100                                                                          
064200       MOVE FELM-DISTR-KUND-SAKNAS TO MED-IDMFSFEL                        
064300       CALL WMEDKONV USING MED-WMEDAREA                                   
064400       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
064500       PERFORM MFS-RENSA-FAELT-UT                                         
064600     END-IF                                                               
064700                                                                          
064800     MOVE '002'     TO MSGI-KDCALL                                        
064900     MOVE '4515'    TO MSGI-IDTRANS                                       
065000     MOVE '4515'    TO SAVE-IDTRANS                                       
065100     MOVE SAVE-AREA TO MSGI-SPAR-AREA                                     
065200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
065300                                                                          
065400     .                                                                    
065500     EJECT                                                                
065600 FA-LAES-BEHANDLA-RADINFO SECTION.                                        
065700                                                                          
065800     MOVE +1 TO RAD-INDX                                                  
065900     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
066000                   BASEN-SLUT     OR                                      
066100                   RAD-INDX > MAX-TABRADER                                
066200                                                                          
066300       PERFORM FAA-KOLLA-OM-RAETT-DC                                      
066400                                                                          
066500       IF IDDC-RAETT                                                      
066600         PERFORM FAB-KOLLA-ORDER-STATUS                                   
066700                                                                          
066800         IF SPAR-KDORDSTA = 'F ' OR 'L ' OR                               
066900                            'L*' OR 'P ' OR                               
067000                            'P*' OR 'U*' OR                               
067100                            'S ' OR 'S*'                                  
067200           PERFORM FAC-VISA-INFO-PA-BILD                                  
067300         END-IF                                                           
067400       END-IF                                                             
067500                                                                          
067600       PERFORM IMS-GN-WDE401-DISTR                                        
067700     END-PERFORM                                                          
067800                                                                          
067900     PERFORM FAD-KOLLA-OM-FLER-SIDOR                                      
068000     .                                                                    
068100     EJECT                                                                
068200 FAA-KOLLA-OM-RAETT-DC SECTION.                                           
068300                                                                          
068400     IF DCS-IDDC = KORD-IDDC                                              
068500                                                                          
068600       MOVE JA TO IDDC-SW                                                 
068700                                                                          
068800     ELSE                                                                 
068900       IF DCS-IDDC = '0'                                                  
069000                                                                          
069100         MOVE JA  TO IDDC-SW                                              
069200                                                                          
069300       ELSE                                                               
069400         MOVE NEJ TO IDDC-SW                                              
069500                                                                          
069600       END-IF                                                             
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 FAB-KOLLA-ORDER-STATUS SECTION.                                          
070100                                                                          
070200     IF KORD-IDPRODNR NOT = SPAR-IDPRODNR                                 
070300       MOVE KORD-IDKUNDNR TO SPAR-IDKUNDNR                                
070400       MOVE KORD-IDORDNR5 TO SPAR-IDORDNR5                                
070500       MOVE KORD-IDDC     TO SPAR-IDDC                                    
070600       MOVE KORD-IDPRODNR TO SPAR-IDPRODNR                                
070700       MOVE KORD-IDPLKLST TO SPAR-IDPLKLST                                
070800       MOVE KORD-IDORDER  TO SPAR-IDORDER                                 
070900                                                                          
071000       MOVE KORD-IDPRODNR TO W-VORD-IDPRODNR                              
071100       PERFORM IMS-GU-WDE601-KOLLI                                        
071200                                                                          
071300       IF VORD-FLDIRLEV = 'N'                                             
071400                                                                          
071500         PERFORM FABA-SPARA-KOLLI-INFO                                    
071600         PERFORM FABB-TA-FRAM-MINIMI-STATUS                               
071700                                                                          
071800       END-IF                                                             
071900     END-IF                                                               
072000     .                                                                    
072100     EJECT                                                                
072200 FABA-SPARA-KOLLI-INFO SECTION.                                           
072300                                                                          
072400     MOVE VORD-KDFRAKT       TO SPAR-KDFRAKT                              
072500     MOVE VORD-KDORDKL       TO SPAR-KDORDKL                              
072600     MOVE VORD-KVKOLPAC      TO SPAR-KVKOLPAC                             
072700     MOVE VORD-KVKOLLI-FAKT  TO SPAR-KVKOLLI-FAKT                         
072800     MOVE VORD-KVKOLLI-LAST  TO SPAR-KVKOLLI-LAST                         
072900     MOVE VORD-VKORDBTO      TO SPAR-VKORDBTO                             
073000     MOVE VORD-VLORDBTO      TO SPAR-VLORDBTO                             
073100     .                                                                    
073200     EJECT                                                                
073300 FABB-TA-FRAM-MINIMI-STATUS SECTION.                                      
073400                                                                          
073500     IF VORD-KDORDSTA < +3                                                
073600                                                                          
073700       PERFORM FABBA-STATUS-UTSKRIVEN                                     
073800                                                                          
073900     ELSE                                                                 
074000       IF VORD-KDORDSTA = +3                                              
074100                                                                          
074200         PERFORM FABBB-STATUS-PACKAD                                      
074300                                                                          
074400       ELSE                                                               
074500         IF VORD-KDORDSTA = +4                                            
074600           PERFORM FABBD-STATUS-SKEPPAD                                   
074700         END-IF                                                           
074800       END-IF                                                             
074900     END-IF                                                               
075000     .                                                                    
075100     EJECT                                                                
075200 FABBA-STATUS-UTSKRIVEN SECTION.                                          
075300                                                                          
075400     IF VORD-KVKOLPAC     = ZERO AND                                      
075500        VORD-KVKOLLI-LAST = ZERO AND                                      
075600        VORD-KVKOLLI-FAKT = ZERO                                          
075700                                                                          
075800       CONTINUE                                                           
075900     ELSE                                                                 
076000                                                                          
076100       MOVE 'U*' TO SPAR-KDORDSTA                                         
076200     END-IF                                                               
076300     .                                                                    
076400     EJECT                                                                
076500 FABBB-STATUS-PACKAD SECTION.                                             
076600                                                                          
076700     IF VORD-KVKOLLI-FL = ZERO                                            
076800                                                                          
076900       MOVE 'P ' TO SPAR-KDORDSTA                                         
077000     ELSE                                                                 
077100                                                                          
077200       MOVE 'P*' TO SPAR-KDORDSTA                                         
077300     END-IF                                                               
077400     .                                                                    
077500     EJECT                                                                
077600 FABBD-STATUS-SKEPPAD SECTION.                                            
077700                                                                          
077800     IF VORD-KVKOLLI-FAKT = ZERO                                          
077900       MOVE 'S ' TO SPAR-KDORDSTA                                         
078000     ELSE                                                                 
078100       IF VORD-KVKOLLI-FAKT < VORD-KVKOLLI                                
078200         MOVE 'S*' TO SPAR-KDORDSTA                                       
078300       ELSE                                                               
078400         CONTINUE                                                         
078500       END-IF                                                             
078600     END-IF                                                               
078700     .                                                                    
078800     EJECT                                                                
078900 FAC-VISA-INFO-PA-BILD SECTION.                                           
079000                                                                          
079100     IF RAD-INDX NOT > MAX-TABRADER                                       
079200                                                                          
079300       MOVE SPAR-IDKUNDNR     TO MOD-IDKUNDNR (RAD-INDX)                  
079400       MOVE SPAR-IDORDNR5     TO MOD-IDORDNR7(RAD-INDX)                   
079500       MOVE SPAR-IDDC         TO MOD-IDDC (RAD-INDX)                      
079600       MOVE SPAR-KDFRAKT      TO MOD-KDFRAKT (RAD-INDX)                   
079700       MOVE SPAR-KDORDKL      TO MOD-KDORDKL (RAD-INDX)                   
079800       MOVE SPAR-KDORDSTA     TO MOD-KDORDSTA (RAD-INDX)                  
079900       MOVE SPAR-KVKOLPAC     TO MOD-KVKOLPAC (RAD-INDX)                  
080000       MOVE SPAR-KVKOLLI-FAKT TO MOD-KVKOLLI-FAKT (RAD-INDX)              
080100       MOVE SPAR-KVKOLLI-LAST TO MOD-KVKOLLI-LAST (RAD-INDX)              
080200       MOVE SPAR-VKORDBTO     TO MOD-VKORDBTO (RAD-INDX)                  
080300       MOVE SPAR-VLORDBTO     TO MOD-VLORDBTO (RAD-INDX)                  
080400                                                                          
080500       IF RAD-INDX = 1                                                    
080600         MOVE SPAR-IDKUNDNR   TO SAVE-IDKUNDNR-PREV(PGNO)                 
080700         MOVE SPAR-IDORDNR5   TO SAVE-IDKUNDRF-PREV(PGNO)                 
080800         MOVE SPAR-IDPRODNR   TO SAVE-IDPRODNR-PREV(PGNO)                 
080900         MOVE SPAR-IDPLKLST   TO SAVE-IDPLKLST-PREV(PGNO)                 
081000       END-IF                                                             
081100                                                                          
081200       ADD +1 TO RAD-INDX                                                 
081300                                                                          
081400     END-IF                                                               
081500                                                                          
081600     MOVE SPACE TO SPAR-KDORDSTA                                          
081700     .                                                                    
081800     EJECT                                                                
081900 FAD-KOLLA-OM-FLER-SIDOR SECTION.                                         
082000                                                                          
082100     IF RAD-INDX > MAX-TABRADER AND SEGMENT-FINNS                         
082200                                                                          
082300       MOVE KORD-IDKUNDNR TO SAVE-IDKUNDNR-NEXT                           
082400       MOVE KORD-IDKUNDRF TO SAVE-IDKUNDRF-NEXT                           
082500       MOVE KORD-IDPRODNR TO SAVE-IDPRODNR-NEXT                           
082600       MOVE KORD-IDPLKLST TO SAVE-IDPLKLST-NEXT                           
082700                                                                          
082800       MOVE INF-MER-INFO-FINNS TO MED-IDMFSINF                            
082900       CALL WMEDKONV USING MED-WMEDAREA                                   
083000       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
083100                                                                          
083200     END-IF                                                               
083300                                                                          
083400     IF SEGMENT-SAKNAS                                                    
083500                                                                          
083600       MOVE INF-LAST-PAGE        TO MED-IDMFSFEL                          
083700       CALL WMEDKONV          USING MED-WMEDAREA                          
083800       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
083900                                                                          
084000     END-IF                                                               
084100     .                                                                    
084200     EJECT                                                                
084300 S01-INSERT-ALTMSG SECTION.                                               
084400                                                                          
084500     MOVE P-TO-P-SW TO MSG-IO-AREA                                        
084600     PERFORM IMS-CHANGE-ALTMSG                                            
084700     IF STATUS-OK                                                         
084800       PERFORM IMS-INSERT-ALTMSG                                          
084900     ELSE                                                                 
085000       MOVE LOW-VALUE          TO MSG-AREA                                
085100       MOVE 'W4O51501'         TO MFS-IDMOD                               
085200       MOVE '4515'             TO MOD-IDTRANS                             
085300       MOVE P-TO-P-KDTRANS (2:1) TO W-BILD (1:1)                          
085400       MOVE P-TO-P-KDTRANS (4:3) TO W-BILD (2:3)                          
085500       IF SECURITY-FEL                                                    
085600         STRING 'NOT AUTHORIZED TO USE '                                  
085700                W-BILD                                                    
085800                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
085900       ELSE                                                               
086000         STRING 'WRONG PICTURE '                                          
086100                 W-BILD                                                   
086200                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
086300       END-IF                                                             
086400       PERFORM MFS-ROER-EJ-BILD                                           
086500       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O51501 + 4                      
086600       PERFORM IMS-INSERT-MSG                                             
086700     END-IF                                                               
086800     .                                                                    
086900     EJECT                                                                
087000                                                                          
087100 MFS-RENSA-FAELT-IN SECTION.                                              
087200                                                                          
087300*    --- ALLA INDATA-FÄLT                                                 
087400                                                                          
087500     MOVE +1 TO RAD-INDX                                                  
087600     PERFORM UNTIL RAD-INDX     >  MAX-TABRADER                           
087700       MOVE MFS-RENSA-FAELT     TO MOD-IDTRANS-RAD(RAD-INDX)              
087800       ADD +1                   TO RAD-INDX                               
087900     END-PERFORM                                                          
088000     .                                                                    
088100     EJECT                                                                
088200 MFS-RENSA-FAELT-UT SECTION.                                              
088300                                                                          
088400*    --- ALLA UTDATA-FÄLT                                                 
088500                                                                          
088600     MOVE +1              TO RAD-INDX                                     
088700     PERFORM UNTIL RAD-INDX > MAX-TABRADER                                
088800       MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR(RAD-INDX)                     
088900                               MOD-IDORDNR7(RAD-INDX)                     
089000                               MOD-KDORDKL(RAD-INDX)                      
089100                               MOD-KDFRAKT(RAD-INDX)                      
089200                               MOD-KDORDSTA(RAD-INDX)                     
089300                               MOD-KVKOLPAC(RAD-INDX)                     
089400                               MOD-KVKOLLI-FAKT(RAD-INDX)                 
089500                               MOD-KVKOLLI-LAST(RAD-INDX)                 
089600                               MOD-VKORDBTO(RAD-INDX)                     
089700                               MOD-VLORDBTO(RAD-INDX)                     
089800       ADD +1 TO RAD-INDX                                                 
089900     END-PERFORM                                                          
090000     .                                                                    
090100     EJECT                                                                
090200                                                                          
090300 MFS-ROER-EJ-BILD   SECTION.                                              
090400                                                                          
090500*    --- ALLA UTDATA-FÄLT                                                 
090600     MOVE +1              TO RAD-INDX                                     
090700     PERFORM UNTIL RAD-INDX > MAX-TABRADER                                
090800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR(RAD-INDX)                   
090900                                 MOD-IDORDNR7(RAD-INDX)                   
091000                                 MOD-IDDC (RAD-INDX)                      
091100                                 MOD-KDORDKL(RAD-INDX)                    
091200                                 MOD-KDFRAKT(RAD-INDX)                    
091300                                 MOD-KDORDSTA(RAD-INDX)                   
091400                                 MOD-KVKOLPAC(RAD-INDX)                   
091500                                 MOD-KVKOLLI-FAKT(RAD-INDX)               
091600                                 MOD-KVKOLLI-LAST(RAD-INDX)               
091700                                 MOD-VKORDBTO(RAD-INDX)                   
091800                                 MOD-VLORDBTO(RAD-INDX)                   
091900       ADD +1 TO RAD-INDX                                                 
092000     END-PERFORM                                                          
092100     .                                                                    
092200     EJECT                                                                
092300* --- IMS SEKTIONER ---                                                   
092400     SKIP3                                                                
092500 IMS-GET-MSG SECTION.                                                     
092600                                                                          
092700     MOVE '  QC' TO GODK-STATUSKODER                                      
092800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
092900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
093000     PERFORM IMS-STATUSKONTROLL                                           
093100     .                                                                    
093200     SKIP3                                                                
093300 IMS-INSERT-MSG SECTION.                                                  
093400                                                                          
093500     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
093600       MOVE '0' TO MFS-KDHUVOMR                                           
093700     END-IF                                                               
093800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
093900     MOVE SPACE TO GODK-STATUSKODER                                       
094000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
094100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094200     PERFORM IMS-STATUSKONTROLL                                           
094300     .                                                                    
094400     EJECT                                                                
094500 IMS-CHANGE-ALTMSG SECTION.                                               
094600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
094700     MOVE '  A1A4' TO GODK-STATUSKODER                                    
094800     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
094900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
095000     PERFORM IMS-STATUSKONTROLL                                           
095100     .                                                                    
095200     SKIP3                                                                
095300 IMS-INSERT-ALTMSG SECTION.                                               
095400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
095500     MOVE SPACE TO GODK-STATUSKODER                                       
095600     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
095700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
095800     PERFORM IMS-STATUSKONTROLL                                           
095900     .                                                                    
096000     EJECT                                                                
096100 IMS-GU-WDE401-DISTR SECTION.                                             
096200                                                                          
096300     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
096400          DELIMITED BY SIZE INTO SSA1                                     
096500     MOVE '  GE' TO GODK-STATUSKODER                                      
096600     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA SSA1                      
096700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
096800     PERFORM IMS-STATUSKONTROLL                                           
096900     .                                                                    
097000     SKIP3                                                                
097100 IMS-GN-WDE401-DISTR SECTION.                                             
097200                                                                          
097300     STRING 'WDE401  (WDE4ASEQ=>' W-WDE4ASEQ-MIN-X                        
097400                    '&WDE4ASEQ=<' W-WDE4ASEQ-MAX-X ')'                    
097500          DELIMITED BY SIZE INTO SSA1                                     
097600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
097700     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-AREA SSA1                      
097800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
097900     PERFORM IMS-STATUSKONTROLL                                           
098000     .                                                                    
098100     EJECT                                                                
098200 IMS-GU-WDE601-KOLLI SECTION.                                             
098300                                                                          
098400     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
098500          DELIMITED BY SIZE INTO SSA1                                     
098600     MOVE '  GE' TO GODK-STATUSKODER                                      
098700     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA SSA1                      
098800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
098900     PERFORM IMS-STATUSKONTROLL                                           
099000     .                                                                    
099100     SKIP3                                                                
099200 IMS-GU-WDB601    SECTION.                                                
099300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
099400          DELIMITED BY SIZE INTO SSA1                                     
099500     MOVE '  GE' TO GODK-STATUSKODER                                      
099600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
099700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
099800     PERFORM IMS-STATUSKONTROLL                                           
099900     IF SEGMENT-SAKNAS                                                    
100000         MOVE SPACE TO DCS-KDDC                                           
100100     END-IF                                                               
100200     .                                                                    
100300 IMS-STATUSKONTROLL SECTION.                                              
100400                                                                          
100500     SET STATUS-IX TO 1                                                   
100600     SEARCH GODK-STATUS                                                   
100700       AT END CALL FELLOG                                                 
100800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
100900     END-SEARCH                                                           
101000     .                                                                    
