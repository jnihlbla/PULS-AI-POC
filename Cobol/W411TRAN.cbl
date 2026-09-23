000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP1                                                                
000400 PROGRAM-ID.     W411TRAN.                                                
000500 AUTHOR.         STEFANO GIOBBI.                                          
000600 DATE-WRITTEN.   MAJ. 90.                                                 
000700*                                                                         
000800*    REMARKS.                                                             
000900*                                                                         
001000*                                                                         
001100*    FUNKTION.                                                            
001200*                                                                         
001300*        PROGRAMMET GÖR I HUVUDSAK EN AV TVÅ SAKER:                       
001400*          1. KONTROLLERAR TIRFS (DATUM + TID)                            
001500*          2. ATT MED UTGÅNGSPUNKT FRÅN ORDERREGISTRERINGSDATUM,          
001600*             -TID OCH LEDTID GE FÖRSLAG PÅ FÖRSTA MÖJLIGA TRANS-         
001700*             PORTAVGÅNGSTIDPUNKT, TAT.                                   
001800*        TAT HÄMTAS I REGISTRET MED TRANSPORTAVGÅNGAR MED NYCKEL          
001900*        IDTRP.                                                           
002000*        NÄR PROGRAMMET BERÄKNAR EN TAT, RÄKNAS FÖRST EN PRELIMI-         
002100*        ÄR TAT FRAM. DEN FÅR MAN GENOM ATT LÄGGA IHOP O-REGTIDEN         
002200*        MED LEDTIDEN FR O M O-REGDATUMET MED HÄNSYN TAGEN TILL           
002300*        LEDIGA, ARBETSFRIA OCH HALVA DAGAR.                              
002400*        PREL TAT ÄR DEN TIDPUNKT DÅ ORDER TIDIGAST KAN SKICKAS           
002500*        IVÄG. AV DE MÖJLIGA TAT SOM LIGGER UNDER ETT GIVET               
002600*        IDTRP VÄLJS DEN UT SOM LIGGER NÄRMAST PREL TAT RÄKNAT            
002700*        FRAMÅT I TIDEN, DENNA TAT BLIR DÅ DEFINITIV TAT.                 
002800*        DET LÅTER VÄL LOGISKT?                                           
002900*                                                                         
003000*        I A-INIT INITIERAS WORKING STORAGE. DETTA INNEBÄR ATT            
003100*        ALLA VARIABLER VARS VÄRDE KAN FÖRÄNDRAS MÅSTE FÖRUTOM            
003200*        ATT DEKLARERAS I WORKING STORAGE ÄVEN INITIERAS I A-INIT.        
003300*                                                                         
003400*                                                                         
003500*    LÄNKAREA: W411TRAN                                                   
003600*                                                                         
003700*    BASER   : FYSISKT   LOGISKT   HÄNDELSETYP                            
003800*              WDR1      WLXXKB    4433, 4434  TRANSPORTAVGÅNGS-          
003900*                                              TIDSREGISTER               
004000*    STORY 2375089 / ADD IDSYSTEM VOUI, ECOM                              
004100*                                                                         
004200     EJECT                                                                
004300 ENVIRONMENT DIVISION.                                                    
004400     SKIP2                                                                
004500 DATA DIVISION.                                                           
004600     SKIP1                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800*    -COPY WY2000WB                                                       
004900     SKIP3                                                                
005000*    -COPY WY2000W9                                                       
005100     SKIP3                                                                
005200*    -COPY WY2000W2                                                       
005300     SKIP3                                                                
005400*    -COPY WY2000W1                                                       
005500     SKIP3                                                                
005600*                                                                         
005700 77  IDPGM                   PIC X(8)    VALUE 'W411TRAN'.                
005800 77  FELTEXT-VID-CALL-ABEND  PIC X(64)   VALUE SPACE.                     
005900 77  WS-SECTION-NAME         PIC X(64)   VALUE SPACE.                     
006000 77  JA                      PIC X       VALUE 'J'.                       
006100 77  NEJ                     PIC X       VALUE 'N'.                       
006200 77  SPRAK-IX                PIC S9(9)   VALUE +0    COMP SYNC.           
006300 77  RKOD-ABEND-MED-DUMP     PIC S9(4)   VALUE +1000 COMP SYNC.           
006400                                                                          
006500 77  ALLT-SW                 PIC  X(1)   VALUE 'J'.                       
006600     88  ALLT-OK                         VALUE 'J'.                       
006700                                                                          
006800 01  GENERELLA-SUBPROGRAM.                                                
006900*                                                                         
007000   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
007100   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
007200   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
007300   03  WZ20DAYS              PIC X(8)    VALUE 'WZ20DAYS'.                
007400   03  WORKDAY               PIC X(8)    VALUE 'WORKDAY '.                
007500     EJECT                                                                
007600 01  SWITCH-AREOR.                                                        
007700*                                                                         
007800   03  SW-PREL-TAT-SATT      PIC X(1)                 VALUE 'N'.          
007900   03  SW-DEF-TAT-SATT       PIC X(1)                 VALUE 'N'.          
008000   03  SW-NAESTA-ARBVECKA-FUNNEN                                          
008100                             PIC X(1)                 VALUE 'N'.          
008200   03  SW-GODK-AARSTRP-FUNNEN                                             
008300                             PIC X(1)                 VALUE 'J'.          
008400     SKIP2                                                                
008500 01  KONSTANT-AREOR.                                                      
008600*                                                                         
008700   03  FILLER                PIC X(16) VALUE 'KONSTANT-AREOR'.            
008800   03  FILLER                PIC X(4)        VALUE 'AAAA'.                
008900   03  K-LEDIG-DAG           PIC S9(1)       COMP-3   VALUE +0.           
009000   03  K-ARBETSDAG           PIC S9(1)       COMP-3   VALUE +1.           
009100   03  K-60-MINUTER          PIC S9(2)       COMP-3   VALUE +60.          
009200   03  K-12-TIMMAR           PIC S9(2)       COMP-3   VALUE +12.          
009300   03  K-24-TIMMAR           PIC S9(2)       COMP-3   VALUE +24.          
009400   03  FILLER                PIC X(4)        VALUE 'BBBB'.                
009500   03  K-KDSVAR-OK-0         PIC  X(1)                VALUE '0'.          
009600   03  K-KDSVAR-TRPID-SAKNAS-1                                            
009700                             PIC  X(1)                VALUE '1'.          
009800   03  K-KDSVAR-RFSDAT-FEL-2 PIC  X(1)                VALUE '2'.          
009900   03  K-KDSVAR-RFSTID-FEL-3 PIC  X(1)                VALUE '3'.          
010000   03  K-KDSVAR-TRPAVT-U-RFS-4                                            
010100                             PIC  X(1)                VALUE '4'.          
010200     EJECT                                                                
010300 01  SPAR-AREOR.                                                          
010400*                                                                         
010500   03  FILLER                PIC X(16)   VALUE 'SPAR-AREOR'.              
010600   03  SPAR-TRAN-LEDTID      PIC S9(3)V9(2)  COMP-3   VALUE +0.           
010700   03  SPAR-TRAN-LEDTID-REST PIC S9(1)       COMP-3   VALUE +0.           
010800   03  SPAR-TRAN-REGTID      PIC S9(3)V9(2)  COMP-3   VALUE +0.           
010900   03  SPAR-TRAN-REGTID-REST PIC S9(1)       COMP-3   VALUE +0.           
011000   03  SPAR-NAESTA-ARBDAG    PIC  9(6)                VALUE ZERO.         
011100   03  SPAR-NAESTA-VECKA     PIC  9(6)                VALUE ZERO.         
011200*                                                                         
011300   03  FILLER                PIC X(4)        VALUE 'CCCC'.                
011400   03  SPAR-DEFINITIV-TAT.                                                
011500       05  SPAR-DEF-TATDAT   PIC  9(6)                VALUE               
011600                                                           999999.        
011700       05  SPAR-DEF-TATTID   PIC S9(5)V9(2)  COMP-3   VALUE               
011800                                                        +99999.99.        
011900   03  SPAR-PRELIMINAER-TAT.                                              
012000       05  SPAR-PREL-TATDAT  PIC  9(6)                VALUE ZERO.         
012100       05  SPAR-PREL-TATTID  PIC S9(5)V9(2)  COMP-3   VALUE +0.           
012200*                                                                         
012300   03  FILLER                PIC X(4)        VALUE 'DDDD'.                
012400   03  SPAR-PREL-TATDAT-AAVVD                                             
012500                             PIC 9(5)                 VALUE ZERO.         
012600   03  FILLER                REDEFINES SPAR-PREL-TATDAT-AAVVD.            
012700       05  SPAR-PTAT-AA      PIC  9(2).                                   
012800       05  SPAR-PTAT-VV      PIC  9(2).                                   
012900       05  SPAR-PTAT-D       PIC  9(1).                                   
013000*                                                                         
013100   03  SPAR-PREL-DEF-TAT.                                                 
013200       05  SPAR-PREL-DEF-TATDAT                                           
013300                             PIC  9(6)                VALUE ZERO.         
013400       05  SPAR-PREL-DEF-TATTID                                           
013500                             PIC S9(5)V9(2)  COMP-3   VALUE +0.           
013600*                                                                         
013700   03  FILLER                PIC X(4)        VALUE 'EEEE'.                
013800   03  SPAR-TITRPAVG-AA      PIC  9(2)                VALUE ZERO.         
013900   03  SPAR-TITRPAVG-TTMM    PIC S9(5)V9(2)  COMP-3   VALUE +0.           
014000*                                                                         
014100   03  SPAR-TITRPAVG         PIC  9(7)                VALUE ZERO.         
014200   03  SPAR-TITRPAVG-VVDTTMM REDEFINES SPAR-TITRPAVG.                     
014300       05  SPAR-TITRPAVG-VV  PIC  9(2).                                   
014400       05  SPAR-TITRPAVG-D   PIC  9(1).                                   
014500       05  SPAR-TITRPAVG-TT  PIC  9(2).                                   
014600       05  SPAR-TITRPAVG-MM  PIC  9(2).                                   
014700*                                                                         
014800   03  FILLER                PIC X(4)        VALUE 'FFFF'.                
014900   03  SPAR-DAGENS-DATUM     PIC  9(6)                VALUE ZERO.         
015000   03  FILLER                REDEFINES SPAR-DAGENS-DATUM.                 
015100       05  SPAR-DAGENS-AA    PIC  9(2).                                   
015200       05  SPAR-DAGENS-MM    PIC  9(2).                                   
015300       05  SPAR-DAGENS-DD    PIC  9(2).                                   
015400*                                                                         
015500   03  SPAR-DAGENS-AAVVD     PIC  9(5)                VALUE ZERO.         
015600   03  FILLER                REDEFINES SPAR-DAGENS-AAVVD.                 
015700       05  FILLER            PIC  9(2).                                   
015800       05  SPAR-DAGENS-VV    PIC  9(2).                                   
015900       05  SPAR-DAGENS-D     PIC  9(1).                                   
016000     EJECT                                                                
016100 01  HELP-AREOR.                                                          
016200*                                                                         
016300   03  FILLER                PIC X(16)   VALUE 'HELP-AREOR'.              
016400   03  HELP-NAESTA-ARBDAG    PIC  9(6)                VALUE ZERO.         
016500   03  HELP-NAESTA-VECKA     PIC  9(6)                VALUE ZERO.         
016600   03  HELP-MIN-TILL-DEC     PIC  9(2)                VALUE ZERO.         
016700*                                                                         
016800   03  HELP-LEDTID           PIC  9(3)V9(2)           VALUE ZERO.         
016900   03  HELP-LEDTID-TIM-MIN REDEFINES HELP-LEDTID.                         
017000       05  HELP-LEDTID-TIM                                                
017100                             PIC  9(3).                                   
017200       05  HELP-LEDTID-MIN                                                
017300                             PIC  9(2).                                   
017400   03  FILLER                PIC X(4)        VALUE 'GGGG'.                
017500*                                                                         
017600   03  HELP-LEDTID-REGTID    PIC  9(5)                VALUE ZERO.         
017700   03  HELP-LEDTID-REGT-TIM-MIN REDEFINES HELP-LEDTID-REGTID.             
017800       05  HELP-LEDTID-REGT-TIM                                           
017900                             PIC  9(3).                                   
018000       05  HELP-LEDTID-REGT-MIN                                           
018100                             PIC  9(2).                                   
018200*                                                                         
018300   03  HELP-TITRPAVG         PIC  9(7)                VALUE ZERO.         
018400   03  FILLER                REDEFINES HELP-TITRPAVG.                     
018500       05  HELP-TITRPAVG-MMAAVVD.                                         
018600           07  FILLER        PIC  9(2).                                   
018700           07  HELP-TITRPAVG-AAVVD.                                       
018800               09  HELP-TITRPAVG-AA                                       
018900                             PIC  9(2).                                   
019000               09  HELP-TITRPAVG-VV                                       
019100                             PIC  9(2).                                   
019200               09  HELP-TITRPAVG-D                                        
019300                             PIC  9(1).                                   
019400*                                                                         
019500   03  FILLER                PIC X(4)        VALUE 'HHHH'.                
019600   03  HELP-TIRFS            PIC  9(11)               VALUE ZERO.         
019700   03  HELP-TIRFS-DAT-TID  REDEFINES HELP-TIRFS.                          
019800       05  FILLER            PIC  9(1).                                   
019900       05  HELP-TIRFS-DAT    PIC  9(6).                                   
020000       05  HELP-TIRFS-TID    PIC  9(2)V9(2).                              
020100*                                                                         
020200   03  HELP-TATTID           PIC  9(5)V9(2)           VALUE ZERO.         
020300   03  FILLER              REDEFINES HELP-TATTID.                         
020400       05  FILLER            PIC  9(3).                                   
020500       05  HELP-TATTID-HHMM.                                              
020600           07  HELP-TATTID-HH                                             
020700                             PIC  9(2).                                   
020800           07  HELP-TATTID-MM                                             
020900                             PIC  9(2).                                   
021000     EJECT                                                                
021100 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
021200*   -COPY WZ20DAYS                                                        
021300     EJECT                                                                
021400 01  FILLER                  PIC X(16)   VALUE 'WORKAREA   '.             
021500*   -COPY WORKAREA                                                        
021600     EJECT                                                                
021700 01  FILLER                  PIC X(16)   VALUE 'VALID DC   '.             
021800*   -COPY WWDC99                                                          
021900     EJECT                                                                
022000******************************************************************        
022100*                                                                *        
022200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                       *        
022300*                                                                *        
022400******************************************************************        
022500*                                                                         
022600 01  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.             
022700     SKIP3                                                                
022800 01  NYCKLAR-TILL-DLI.                                                    
022900*                                                                         
023000   03  W-WDGXKEY-4433-X.                                                  
023100       05  W-4433-IDHTYP       PIC  X(4)        VALUE '4433'.             
023200       05  W-4433-IDDC         PIC  X(2)        VALUE SPACE.              
023300       05  FILLER              PIC  X(24)       VALUE LOW-VALUE.          
023400*                                                                         
023500   03  W-WDGXKEY-4434-MIN-X.                                              
023600       05  W-4434-IDTRP-MIN    PIC  X(5)        VALUE SPACE.              
023700       05  FILLER              PIC  X(5)        VALUE LOW-VALUE.          
023800*                                                                         
023900   03  W-WDGXKEY-4434-MAX-X.                                              
024000       05  W-4434-IDTRP-MAX    PIC  X(5)        VALUE SPACE.              
024100       05  FILLER              PIC  X(5)        VALUE HIGH-VALUE.         
024200     EJECT                                                                
024300******************************************************************        
024400*                                                                *        
024500*        STATUSKODER FRÅN IMS                                    *        
024600*                                                                *        
024700******************************************************************        
024800*                                                                         
024900   03  FILLER                  PIC X(16) VALUE 'STATUSKODER'.             
025000   03  STATUS-WS               PIC X(2).                                  
025100*                                                                         
025200     88  SEGMENT-FINNS                   VALUE '  '.                      
025300     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
025400     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
025500     88  BASEN-SLUT                      VALUE 'GB'.                      
025600     SKIP3                                                                
025700   03  GODK-STATUSKODER.                                                  
025800     05  GODK-STATUS OCCURS  5                                            
025900                     INDEXED BY STATUS-IX PIC X(2).                       
026000     SKIP3                                                                
026100******************************************************************        
026200*                                                                *        
026300*        SSA:ER                                                  *        
026400*                                                                *        
026500******************************************************************        
026600*                                                                         
026700 01  SSA1-FILLER             PIC X(16) VALUE 'SSA1       '.               
026800 01  SSA1                    PIC X(128).                                  
026900     EJECT                                                                
027000******************************************************************        
027100*                                                                *        
027200*        IMS FUNKTIONSKODER                                      *        
027300*                                                                *        
027400******************************************************************        
027500*                                                                         
027600*01    -COPY W0003                                                        
027700     EJECT                                                                
027800******************************************************************        
027900*                                                                *        
028000*        DLI INPUT-OUTPUT AREA                                   *        
028100*                                                                *        
028200******************************************************************        
028300*                                                                         
028400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA'.              
028500 01  DLI-IO-AREA.                                                         
028600   03  IO-AREA               PIC X(100) VALUE SPACE.                      
028700     SKIP3                                                                
028800*  03  WLXXKB11  -COPY WDGX4434            -RED IO-AREA.                  
028900     EJECT                                                                
029000*                                                                         
029100 LINKAGE SECTION.                                                         
029200     SKIP2                                                                
029300*             -COPY W411TRAN.                                             
029400     EJECT                                                                
029500*01  -COPY W0008     -PRE XXKB-                                           
029600     05  FILLER              PIC X.                                       
029700     EJECT                                                                
029800 PROCEDURE DIVISION USING TRAN-W411TRAN                                   
029900                          XXKB-PCB.                                       
030000                                                                          
030100     PERFORM A-INIT                                                       
030200                                                                          
030300     PERFORM I-KOLLA-OM-TRPAVT-SKALL-SATTAS                               
030400                                                                          
030500     IF ALLT-OK                                                           
030600       IF TRAN-IDSYSTEM = 'LDCR'                                          
030700         MOVE TRAN-TIRFS TO HELP-TIRFS                                    
030800         PERFORM S04-BESTAM-PREL-TAT-FRAN-TIRFS                           
030900         PERFORM G-BESTAEM-DEFINITIV-TAT                                  
031000       ELSE                                                               
031100         IF TRAN-TIRFS     = ZERO        OR                               
031200           (TRAN-TIRFS     > ZERO        AND                              
031300            TRAN-KDORDKL   > +1 )        OR                               
031400           (TRAN-TIRFS     > ZERO        AND                              
031500            TRAN-KDORDKL   = +1          AND                              
031600            TRAN-IDSYSTEM  = 'LDC ')                                      
031700           PERFORM B-TA-REDA-PAA-LEDTID                                   
031800           PERFORM C-GOER-MIN-TILL-DEC                                    
031900           PERFORM D-BESTAEM-PRELIMINAER-TAT                              
032000           PERFORM E-GOER-DEC-TILL-MIN                                    
032100           IF TRAN-KDTRPKAT = 'A'                                         
032200             IF TRAN-TIRFS > ZERO        AND                              
032300                TRAN-KDORDKL > +1        OR                               
032400                (TRAN-TIRFS > ZERO       AND                              
032500                 TRAN-KDORDKL   = +1     AND                              
032600                 TRAN-IDSYSTEM  = 'LDC ')                                 
032700               PERFORM F-KONTROLLERA-RFS-TRPKAT-A                         
032800             END-IF                                                       
032900             PERFORM G-BESTAEM-DEFINITIV-TAT                              
033000           ELSE                                                           
033100             PERFORM H-KONTROLLERA-RFS-TRPKAT-B-C                         
033200           END-IF                                                         
033300         ELSE                                                             
033400           IF TRAN-IDSYSTEM = 'LDC ' AND CDC                              
033500              PERFORM S13-CALL-WORKDAY-LDC                                
033510              MOVE ZERO      TO HELP-TIRFS                                
033600              MOVE WORK-TIAAMMDD-NEXT-WORKDAY                             
033610                             TO HELP-TIRFS (2:6)                          
033700           ELSE                                                           
033800             MOVE TRAN-TIRFS TO HELP-TIRFS                                
033900           END-IF                                                         
034000                                                                          
034100           PERFORM S04-BESTAM-PREL-TAT-FRAN-TIRFS                         
034200           PERFORM G-BESTAEM-DEFINITIV-TAT                                
034300         END-IF                                                           
034400       END-IF                                                             
034500     END-IF                                                               
034600                                                                          
034700     IF TRAN-TIAAMMDD NOT NUMERIC OR                                      
034800        TRAN-TIHHMM   NOT NUMERIC                                         
034900       MOVE 'PGMFEL, KORREKT TITRPAVT KUNDE EJ SÄTTAS.'                   
035000                               TO FELTEXT-VID-CALL-ABEND                  
035100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
035200     END-IF                                                               
035300                                                                          
035400     MOVE   ZERO TO RETURN-CODE                                           
035500     GOBACK                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 A-INIT SECTION.                                                          
035900                                                                          
036000     MOVE K-KDSVAR-OK-0    TO TRAN-KDSVAR                                 
036100     MOVE ZERO             TO TRAN-TIAAMMDD                               
036200                              TRAN-TIHHMM                                 
036300     MOVE TRAN-IDDC        TO WS-IDDC                                     
036400                                                                          
036500     PERFORM AA-INIT-SWITCH-SPAR-O-HELP                                   
036600                                                                          
036700     ACCEPT SPAR-DAGENS-DATUM FROM DATE                                   
036800     MOVE   SPAR-DAGENS-AA TO SPAR-TITRPAVG-AA                            
036900                                                                          
037000     MOVE  'YYMMDD'            TO DAYS-KDDATFMT1                          
037100     MOVE  'YYWWD'             TO DAYS-KDDATFMT2                          
037200     MOVE   SPAR-DAGENS-DATUM  TO DAYS-TIDATE1                            
037300     MOVE   SPACE              TO DAYS-TIDATE2                            
037400                                  DAYS-IDCALEND                           
037500     CALL     WZ20DAYS USING DAYS-WZ20DAYS                                
037600                                                                          
037700     MOVE     DAYS-TIDATE2(1:5)          TO SPAR-DAGENS-AAVVD             
037800     .                                                                    
037900     EJECT                                                                
038000 AA-INIT-SWITCH-SPAR-O-HELP SECTION.                                      
038100                                                                          
038200     MOVE NEJ       TO                                                    
038300                       SW-PREL-TAT-SATT                                   
038400                       SW-DEF-TAT-SATT                                    
038500                       SW-NAESTA-ARBVECKA-FUNNEN                          
038600                                                                          
038700     MOVE JA        TO SW-GODK-AARSTRP-FUNNEN                             
038800                                                                          
038900     MOVE    999999 TO SPAR-DEF-TATDAT                                    
039000     MOVE +99999.99 TO SPAR-DEF-TATTID                                    
039100                                                                          
039200     MOVE ZERO      TO                                                    
039300                       SPAR-TRAN-LEDTID                                   
039400                       SPAR-TRAN-LEDTID-REST                              
039500                       SPAR-TRAN-REGTID                                   
039600                       SPAR-TRAN-REGTID-REST                              
039700                       SPAR-NAESTA-ARBDAG                                 
039800                       SPAR-NAESTA-VECKA                                  
039900                       SPAR-PREL-TATDAT                                   
040000                       SPAR-PREL-TATTID                                   
040100                       SPAR-PREL-TATDAT-AAVVD                             
040200                       SPAR-PREL-DEF-TATDAT                               
040300                       SPAR-PREL-DEF-TATTID                               
040400                       SPAR-TITRPAVG-AA                                   
040500                       SPAR-TITRPAVG-TTMM                                 
040600                       SPAR-TITRPAVG                                      
040700                       SPAR-DAGENS-DATUM                                  
040800                       SPAR-DAGENS-AAVVD                                  
040900                                                                          
041000                       HELP-NAESTA-ARBDAG                                 
041100                       HELP-NAESTA-VECKA                                  
041200                       HELP-MIN-TILL-DEC                                  
041300                       HELP-LEDTID                                        
041400                       HELP-TITRPAVG                                      
041500                       HELP-TIRFS                                         
041600                       HELP-TATTID                                        
041700     .                                                                    
041800     EJECT                                                                
041900 B-TA-REDA-PAA-LEDTID SECTION.                                            
042000                                                                          
042100     IF TRAN-KDORDKL = +0                                                 
042200       MOVE TRAN-KVLEDTIM-0       TO SPAR-TRAN-LEDTID                     
042300     ELSE                                                                 
042400       IF TRAN-KDORDKL = +1                                               
042500         MOVE TRAN-KVLEDTIM-1     TO SPAR-TRAN-LEDTID                     
042600       ELSE                                                               
042700         IF TRAN-KDORDKL = +2                                             
042800           MOVE TRAN-KVLEDTIM-2   TO SPAR-TRAN-LEDTID                     
042900         ELSE                                                             
043000           IF TRAN-KDORDKL = +3                                           
043100             MOVE TRAN-KVLEDTIM-3 TO SPAR-TRAN-LEDTID                     
043200           ELSE                                                           
043300             MOVE TRAN-KVLEDTIM-4 TO SPAR-TRAN-LEDTID                     
043400           END-IF                                                         
043500         END-IF                                                           
043600       END-IF                                                             
043700     END-IF                                                               
043800     .                                                                    
043900     EJECT                                                                
044000 C-GOER-MIN-TILL-DEC SECTION.                                             
044100                                                                          
044200     MOVE    SPAR-TRAN-LEDTID      TO HELP-LEDTID                         
044300     COMPUTE HELP-MIN-TILL-DEC     =  HELP-LEDTID-MIN   / 6               
044400     COMPUTE SPAR-TRAN-LEDTID-REST =  HELP-LEDTID-MIN   -                 
044500                                     (HELP-MIN-TILL-DEC * 6)              
044600     COMPUTE HELP-LEDTID-MIN       =  HELP-MIN-TILL-DEC * 10              
044700     MOVE    HELP-LEDTID           TO SPAR-TRAN-LEDTID                    
044800                                                                          
044900     MOVE    TRAN-TIHHMM-REG       TO HELP-LEDTID-REGTID                  
045000     MOVE    HELP-LEDTID-REGT-TIM  TO HELP-LEDTID-TIM                     
045100     MOVE    HELP-LEDTID-REGT-MIN  TO HELP-LEDTID-MIN                     
045200     COMPUTE HELP-MIN-TILL-DEC     =  HELP-LEDTID-MIN   / 6               
045300     COMPUTE SPAR-TRAN-REGTID-REST =  HELP-LEDTID-MIN   -                 
045400                                     (HELP-MIN-TILL-DEC * 6)              
045500     COMPUTE HELP-LEDTID-MIN       =  HELP-MIN-TILL-DEC * 10              
045600     MOVE    HELP-LEDTID           TO SPAR-TRAN-REGTID                    
045700     .                                                                    
045800     EJECT                                                                
045900 D-BESTAEM-PRELIMINAER-TAT SECTION.                                       
046000                                                                          
046100     MOVE 'D-BESTAEM-...'  TO WS-SECTION-NAME                             
046200     COMPUTE SPAR-PREL-TATTID = SPAR-TRAN-LEDTID +                        
046300                                SPAR-TRAN-REGTID                          
046400     MOVE    TRAN-TIREGDAT TO    WORK-TIAAMMDD-FOM                        
046500                                 WORK-TIAAMMDD-TOM                        
046600     PERFORM S11-CALL-WORKDAY                                             
046700     PERFORM DA-KOLLA-OM-REGTID-SKALL-ANV                                 
046800                                                                          
046900     MOVE    NEJ           TO SW-PREL-TAT-SATT                            
047000     PERFORM UNTIL SW-PREL-TAT-SATT = JA                                  
047100                                                                          
047200       IF WORK-KVWORKD = K-ARBETSDAG                                      
047300         IF SPAR-PREL-TATTID > K-24-TIMMAR                                
047400           SUBTRACT K-24-TIMMAR FROM SPAR-PREL-TATTID                     
047500           MOVE WORK-TIAAMMDD-NEXT-WORKDAY TO WORK-TIAAMMDD-FOM           
047600                                              WORK-TIAAMMDD-TOM           
047700           PERFORM S11-CALL-WORKDAY                                       
047800         ELSE                                                             
047900           MOVE WORK-TIAAMMDD-FOM TO SPAR-PREL-TATDAT                     
048000           MOVE JA                TO SW-PREL-TAT-SATT                     
048100         END-IF                                                           
048200       ELSE                                                               
048300         MOVE    WORK-TIAAMMDD-NEXT-WORKDAY TO WORK-TIAAMMDD-FOM          
048400                                               WORK-TIAAMMDD-TOM          
048500         PERFORM S11-CALL-WORKDAY                                         
048600       END-IF                                                             
048700     END-PERFORM                                                          
048800                                                                          
048900     PERFORM S06-SPARA-FALT-VID-PTAT-TIDPKT                               
049000     .                                                                    
049100     EJECT                                                                
049200 DA-KOLLA-OM-REGTID-SKALL-ANV SECTION.                                    
049300                                                                          
049400     IF WORK-KVWORKD = K-LEDIG-DAG                                        
049500       COMPUTE SPAR-PREL-TATTID = SPAR-PREL-TATTID -                      
049600                                  SPAR-TRAN-REGTID                        
049700     END-IF                                                               
049800     .                                                                    
049900     EJECT                                                                
050000 E-GOER-DEC-TILL-MIN SECTION.                                             
050100                                                                          
050200     MOVE 'E-GOER-DEC-..'  TO WS-SECTION-NAME                             
050300     MOVE    SPAR-PREL-TATTID TO HELP-LEDTID                              
050400     COMPUTE HELP-LEDTID-MIN = ((HELP-LEDTID-MIN / 10) * 6)               
050500                               + SPAR-TRAN-LEDTID-REST                    
050600                               + SPAR-TRAN-REGTID-REST                    
050700     IF HELP-LEDTID-MIN NOT < K-60-MINUTER                                
050800       ADD      +1           TO   HELP-LEDTID-TIM                         
050900       SUBTRACT K-60-MINUTER FROM HELP-LEDTID-MIN                         
051000       IF HELP-LEDTID-TIM NOT < K-24-TIMMAR                               
051100         SUBTRACT K-24-TIMMAR FROM HELP-LEDTID-TIM                        
051200         MOVE     WORK-TIAAMMDD-NEXT-WORKDAY TO WORK-TIAAMMDD-FOM         
051300                                                WORK-TIAAMMDD-TOM         
051400         PERFORM  S11-CALL-WORKDAY                                        
051500         MOVE     WORK-TIAAMMDD-FOM          TO SPAR-PREL-TATDAT          
051600                                                                          
051700         MOVE     WORK-TIAAMMDD-NEXT-WORKDAY TO SPAR-NAESTA-ARBDAG        
051800         MOVE     WORK-TIAAMMDD-NEXT-WEEK    TO SPAR-NAESTA-VECKA         
051900         MOVE     'YYMMDD'                   TO DAYS-KDDATFMT1            
052000         MOVE     'YYWWD'                    TO DAYS-KDDATFMT2            
052100         MOVE     SPAR-PREL-TATDAT           TO DAYS-TIDATE1              
052200         MOVE     SPACE                      TO DAYS-TIDATE2              
052300                                                DAYS-IDCALEND             
052400         CALL     WZ20DAYS USING DAYS-WZ20DAYS                            
052500                                                                          
052600         MOVE     DAYS-TIDATE2(1:5)          TO                           
052700                                            SPAR-PREL-TATDAT-AAVVD        
052800       END-IF                                                             
052900     END-IF                                                               
053000     MOVE         HELP-LEDTID                TO SPAR-PREL-TATTID          
053100     .                                                                    
053200     EJECT                                                                
053300 F-KONTROLLERA-RFS-TRPKAT-A SECTION.                                      
053400                                                                          
053500     MOVE TRAN-TIRFS TO HELP-TIRFS                                        
053600     IF (TRAN-IDSYSTEM = 'LDC '  OR 'LDCB' OR 'LDCD' OR 'LYNB' OR         
053700                        'LYND' OR 'LYNK' OR                               
053800                        'ECOM' OR 'ECOB' OR 'ECOD' OR                     
053900                        'VOUI' OR 'VOUB' OR 'VOUD' OR                     
054000                        'TAD ' OR 'TADB' OR 'TADD' OR                     
054010                        'ACC ' OR 'ACCB' OR 'ACCD' OR                     
054020                        'APA ' OR 'APAB' OR 'APAD' OR                     
054030                        'APB ' OR 'APBB' OR 'APBD' OR                     
054040                        'APC ' OR 'APCB' OR 'APCD' OR                     
054050                        'APD ' OR 'APDB' OR 'APDD' OR                     
054060                        'APE ' OR 'APEB' OR 'APED' OR                     
054070                        'APF ' OR 'APFB' OR 'APFD' OR                     
054080                        'APG ' OR 'APGB' OR 'APGD' OR                     
054090                        'APH ' OR 'APHB' OR 'APHD' OR                     
054091                        'API ' OR 'APIB' OR 'APID' OR                     
054092                        'APJ ' OR 'APJB' OR 'APJD' )                      
054100       PERFORM S04-BESTAM-PREL-TAT-FRAN-TIRFS                             
054200     ELSE                                                                 
054300       MOVE HELP-TIRFS-DAT   TO TMP1-YYMMDD                               
054400       MOVE SPAR-PREL-TATDAT TO TMP2-YYMMDD                               
054500       PERFORM WY2000P1                                                   
054600       IF TMP1-YYMMDD < TMP2-YYMMDD OR                                    
054700         (TMP1-YYMMDD = TMP2-YYMMDD AND                                   
054800          HELP-TIRFS-TID < SPAR-PREL-TATTID)                              
054900         MOVE  K-KDSVAR-TRPAVT-U-RFS-4 TO TRAN-KDSVAR                     
055000       ELSE                                                               
055100         PERFORM S04-BESTAM-PREL-TAT-FRAN-TIRFS                           
055200       END-IF                                                             
055300     END-IF                                                               
055400     .                                                                    
055500     EJECT                                                                
055600 G-BESTAEM-DEFINITIV-TAT SECTION.                                         
055700                                                                          
055800     MOVE    ZERO             TO HELP-LEDTID                              
055900     MOVE    TRAN-IDDC        TO W-4433-IDDC                              
056000     PERFORM IMS-GU-XXKB01                                                
056100     MOVE    TRAN-IDTRP       TO W-4434-IDTRP-MIN                         
056200                                 W-4434-IDTRP-MAX                         
056300     PERFORM IMS-GNP-XXKB11                                               
056400                                                                          
056500     IF SEGMENT-SAKNAS                                                    
056600       MOVE K-KDSVAR-TRPID-SAKNAS-1 TO TRAN-KDSVAR                        
056700       MOVE ZERO                    TO TRAN-TIAAMMDD                      
056800                                       TRAN-TIHHMM                        
056900*      CALL ABEND USING RKOD-ABEND-MED-DUMP                               
057000     ELSE                                                                 
057100                                                                          
057200       PERFORM UNTIL SEGMENT-SAKNAS  OR                                   
057300                     SW-DEF-TAT-SATT = JA                                 
057400                                                                          
057500         MOVE  4434-TITRPAVG    TO SPAR-TITRPAVG                          
057600         MOVE  SPAR-TITRPAVG-TT TO HELP-LEDTID-TIM                        
057700         MOVE  SPAR-TITRPAVG-MM TO HELP-LEDTID-MIN                        
057800         MOVE  HELP-LEDTID      TO SPAR-TITRPAVG-TTMM                     
057900                                                                          
058000         IF SPAR-TITRPAVG-VV = ZERO AND                                   
058100            SPAR-TITRPAVG-D  = ZERO                                       
058200           PERFORM GA-KONV-DAGTRP-TILL-DATUM                              
058300         ELSE                                                             
058400                                                                          
058500           IF SPAR-TITRPAVG-VV = ZERO                                     
058600             PERFORM GB-KONV-VECKOTRP-TILL-DATUM                          
058700           ELSE                                                           
058800                                                                          
058900             PERFORM GC-KONV-AARSTRP-TILL-DATUM                           
059000           END-IF                                                         
059100         END-IF                                                           
059200                                                                          
059300         IF SW-DEF-TAT-SATT = NEJ                                         
059400           MOVE SPAR-PREL-DEF-TATDAT TO TMP1-YYMMDD                       
059500           MOVE SPAR-DEF-TATDAT      TO TMP2-YYMMDD                       
059600           PERFORM WY2000P1                                               
059700           IF TMP1-YYMMDD < TMP2-YYMMDD  OR                               
059800              SPAR-PREL-DEF-TATTID < SPAR-DEF-TATTID                      
059900             IF SW-GODK-AARSTRP-FUNNEN = NEJ                              
060000*  *  *  SW-GODK-AARSTRP-FUNNEN SÄTTS ENDAST TILL NEJ OM PGM'ET           
060100*  *  *  JOBBAT MED ÅRSTRANSPORTER.                                       
060200               CONTINUE                                                   
060300             ELSE                                                         
060400               MOVE SPAR-PREL-DEF-TAT TO SPAR-DEFINITIV-TAT               
060500             END-IF                                                       
060600           END-IF                                                         
060700                                                                          
060800           PERFORM IMS-GNP-XXKB11                                         
060900         END-IF                                                           
061000       END-PERFORM                                                        
061100                                                                          
061200       IF SEGMENT-SAKNAS                   AND                            
061300           SPAR-DEF-TATDAT        = 999999 AND                            
061400           SW-GODK-AARSTRP-FUNNEN = NEJ                                   
061500         MOVE 'SECT G-, KORR TITRPAVT KUNDE EJ SÄTTAS (ÅRSTRP).'          
061600                               TO FELTEXT-VID-CALL-ABEND                  
061700         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
061800       ELSE                                                               
061900         MOVE    SPAR-DEF-TATDAT      TO TRAN-TIAAMMDD                    
062000         COMPUTE TRAN-TIHHMM          =  SPAR-DEF-TATTID * 100            
062100       END-IF                                                             
062200                                                                          
062300     END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600 GA-KONV-DAGTRP-TILL-DATUM SECTION.                                       
062700                                                                          
062800     MOVE 'GA-KONV-DAGTRP-..'  TO WS-SECTION-NAME                         
062900     IF SPAR-TITRPAVG-TTMM < SPAR-PREL-TATTID                             
063000       MOVE    SPAR-NAESTA-ARBDAG TO WORK-TIAAMMDD-FOM                    
063100                                     WORK-TIAAMMDD-TOM                    
063200       PERFORM S11-CALL-WORKDAY                                           
063300       MOVE WORK-TIAAMMDD-FOM  TO SPAR-PREL-DEF-TATDAT                    
063400       MOVE SPAR-TITRPAVG-TTMM TO SPAR-PREL-DEF-TATTID                    
063500     ELSE                                                                 
063600       MOVE SPAR-PREL-TATDAT   TO SPAR-PREL-DEF-TATDAT                    
063700       MOVE SPAR-TITRPAVG-TTMM TO SPAR-PREL-DEF-TATTID                    
063800                                                                          
063900       IF SPAR-TITRPAVG-TTMM = SPAR-PREL-TATTID                           
064000         MOVE SPAR-PREL-TATDAT TO SPAR-DEF-TATDAT                         
064100         MOVE SPAR-PREL-TATTID TO SPAR-DEF-TATTID                         
064200         MOVE JA               TO SW-DEF-TAT-SATT                         
064300       END-IF                                                             
064400     END-IF                                                               
064500     .                                                                    
064600     EJECT                                                                
064700 GB-KONV-VECKOTRP-TILL-DATUM SECTION.                                     
064800                                                                          
064900     MOVE 'GB-KONV-VECKOT-..'  TO WS-SECTION-NAME                         
065000     IF SPAR-TITRPAVG-D     < SPAR-PTAT-D  OR                             
065100       (SPAR-TITRPAVG-D     = SPAR-PTAT-D  AND                            
065200        SPAR-TITRPAVG-TTMM  < SPAR-PREL-TATTID)                           
065300                                                                          
065400       MOVE    SPAR-NAESTA-VECKA  TO WORK-TIAAMMDD-NEXT-WEEK              
065500       PERFORM S02-TA-FRAM-NAESTA-ARBVECKA                                
065600       MOVE    DAYS-TIDATE2(1:6)  TO SPAR-PREL-DEF-TATDAT                 
065700       MOVE    SPAR-TITRPAVG-TTMM TO SPAR-PREL-DEF-TATTID                 
065800                                                                          
065900     ELSE                                                                 
066000       IF SPAR-TITRPAVG-D    > SPAR-PTAT-D  OR                            
066100         (SPAR-TITRPAVG-D    = SPAR-PTAT-D  AND                           
066200          SPAR-TITRPAVG-TTMM > SPAR-PREL-TATTID)                          
066300         MOVE SPAR-PTAT-AA        TO HELP-TITRPAVG-AA                     
066400         MOVE SPAR-PTAT-VV        TO HELP-TITRPAVG-VV                     
066500         MOVE SPAR-TITRPAVG-D     TO HELP-TITRPAVG-D                      
066600                                                                          
066700         MOVE     'YYWWD'                    TO DAYS-KDDATFMT1            
066800         MOVE     'YYMMDD'                   TO DAYS-KDDATFMT2            
066900         MOVE     HELP-TITRPAVG-AAVVD        TO DAYS-TIDATE1              
067000         MOVE     SPACE                      TO DAYS-TIDATE2              
067100                                                DAYS-IDCALEND             
067200         CALL     WZ20DAYS USING DAYS-WZ20DAYS                            
067300                                                                          
067400         MOVE     DAYS-TIDATE2(1:6)          TO                           
067500                                            WORK-TIAAMMDD-FOM             
067600                                            WORK-TIAAMMDD-TOM             
067700           PERFORM S11-CALL-WORKDAY                                       
067800         IF WORK-KVWORKD       = K-LEDIG-DAG                              
067900           PERFORM S02-TA-FRAM-NAESTA-ARBVECKA                            
068000           MOVE DAYS-TIDATE2(1:6)   TO SPAR-PREL-DEF-TATDAT               
068100           MOVE SPAR-TITRPAVG-TTMM  TO SPAR-PREL-DEF-TATTID               
068200         ELSE                                                             
068300           MOVE DAYS-TIDATE2(1:6)   TO SPAR-DEF-TATDAT                    
068400           MOVE SPAR-TITRPAVG-TTMM  TO SPAR-DEF-TATTID                    
068500           MOVE JA                  TO SW-DEF-TAT-SATT                    
068600         END-IF                                                           
068700       ELSE                                                               
068800         MOVE SPAR-PREL-TATDAT    TO SPAR-DEF-TATDAT                      
068900         MOVE SPAR-PREL-TATTID    TO SPAR-DEF-TATTID                      
069000         MOVE JA                  TO SW-DEF-TAT-SATT                      
069100       END-IF                                                             
069200     END-IF                                                               
069300     .                                                                    
069400     EJECT                                                                
069500 GC-KONV-AARSTRP-TILL-DATUM SECTION.                                      
069600                                                                          
069700     MOVE JA                     TO SW-GODK-AARSTRP-FUNNEN                
069800                                                                          
069900     MOVE SPAR-TITRPAVG-AA   TO TMP1-YY                                   
070000     MOVE SPAR-PTAT-AA       TO TMP2-YY                                   
070100     PERFORM WY2000P9                                                     
070200     IF TMP1-YY < TMP2-YY OR                                              
070300      ( SPAR-TITRPAVG-AA   = SPAR-PTAT-AA       AND                       
070400        SPAR-TITRPAVG-VV   < SPAR-PTAT-VV     ) OR                        
070500      ( SPAR-TITRPAVG-AA   = SPAR-PTAT-AA       AND                       
070600        SPAR-TITRPAVG-VV   = SPAR-PTAT-VV       AND                       
070700        SPAR-TITRPAVG-D    < SPAR-PTAT-D      ) OR                        
070800      ( SPAR-TITRPAVG-AA   = SPAR-PTAT-AA       AND                       
070900        SPAR-TITRPAVG-VV   = SPAR-PTAT-VV       AND                       
071000        SPAR-TITRPAVG-D    = SPAR-PTAT-D        AND                       
071100        SPAR-TITRPAVG-TTMM < SPAR-PREL-TATTID )                           
071200       MOVE    SPAR-TITRPAVG-AA TO HELP-TITRPAVG-AA                       
071300       MOVE    SPAR-TITRPAVG-VV TO HELP-TITRPAVG-VV                       
071400       MOVE    SPAR-TITRPAVG-D  TO HELP-TITRPAVG-D                        
071500                                                                          
071600       PERFORM S03-TA-FRAM-NAESTA-AARSTRP                                 
071700                                                                          
071800       IF SW-GODK-AARSTRP-FUNNEN = JA                                     
071900         MOVE DAYS-TIDATE2(1:6)  TO SPAR-PREL-DEF-TATDAT                  
072000         MOVE SPAR-TITRPAVG-TTMM TO SPAR-PREL-DEF-TATTID                  
072100       END-IF                                                             
072200                                                                          
072300     ELSE                                                                 
072400                                                                          
072500       MOVE SPAR-TITRPAVG-AA   TO TMP1-YY                                 
072600       MOVE SPAR-PTAT-AA       TO TMP2-YY                                 
072700       PERFORM WY2000P9                                                   
072800       IF TMP1-YY > TMP2-YY OR                                            
072900        ( SPAR-TITRPAVG-AA   = SPAR-PTAT-AA       AND                     
073000          SPAR-TITRPAVG-VV   > SPAR-PTAT-VV     ) OR                      
073100        ( SPAR-TITRPAVG-AA   = SPAR-PTAT-AA       AND                     
073200          SPAR-TITRPAVG-VV   = SPAR-PTAT-VV       AND                     
073300          SPAR-TITRPAVG-D    > SPAR-PTAT-D      ) OR                      
073400        ( SPAR-TITRPAVG-AA   = SPAR-PTAT-AA       AND                     
073500          SPAR-TITRPAVG-VV   = SPAR-PTAT-VV       AND                     
073600          SPAR-TITRPAVG-D    = SPAR-PTAT-D        AND                     
073700          SPAR-TITRPAVG-TTMM > SPAR-PREL-TATTID )                         
073800         MOVE    SPAR-TITRPAVG-AA  TO HELP-TITRPAVG-AA                    
073900         MOVE    SPAR-TITRPAVG-VV  TO HELP-TITRPAVG-VV                    
074000         MOVE    SPAR-TITRPAVG-D   TO HELP-TITRPAVG-D                     
074100                                                                          
074200         PERFORM GCA-AARSTRP-AVGAAR-EFTER-PTAT                            
074300                                                                          
074400         IF SW-GODK-AARSTRP-FUNNEN = JA                                   
074500           MOVE DAYS-TIDATE2(1:6)  TO SPAR-PREL-DEF-TATDAT                
074600           MOVE SPAR-TITRPAVG-TTMM TO SPAR-PREL-DEF-TATTID                
074700         END-IF                                                           
074800                                                                          
074900       ELSE                                                               
075000                                                                          
075100         MOVE SPAR-PREL-TATDAT     TO SPAR-DEF-TATDAT                     
075200         MOVE SPAR-PREL-TATTID     TO SPAR-DEF-TATTID                     
075300         MOVE JA                   TO SW-DEF-TAT-SATT                     
075400       END-IF                                                             
075500     END-IF                                                               
075600     .                                                                    
075700     EJECT                                                                
075800 GCA-AARSTRP-AVGAAR-EFTER-PTAT SECTION.                                   
075900                                                                          
076000     MOVE 'GCA-AARSTRP-AVGA.'  TO WS-SECTION-NAME                         
076100     MOVE     'YYWWD'                    TO DAYS-KDDATFMT1                
076200     MOVE     'YYMMDD'                   TO DAYS-KDDATFMT2                
076300     MOVE     HELP-TITRPAVG-AAVVD        TO DAYS-TIDATE1                  
076400     MOVE     SPACE                      TO DAYS-TIDATE2                  
076500                                            DAYS-IDCALEND                 
076600     CALL     WZ20DAYS USING DAYS-WZ20DAYS                                
076700                                                                          
076800     MOVE     DAYS-TIDATE2(1:6)          TO WORK-TIAAMMDD-FOM             
076900                                            WORK-TIAAMMDD-TOM             
077000     PERFORM S11-CALL-WORKDAY                                             
077100     IF WORK-KVWORKD       = K-LEDIG-DAG                                  
077200       PERFORM S03-TA-FRAM-NAESTA-AARSTRP                                 
077300     END-IF                                                               
077400     .                                                                    
077500     EJECT                                                                
077600 H-KONTROLLERA-RFS-TRPKAT-B-C SECTION.                                    
077700                                                                          
077800     MOVE TRAN-TIRFS TO HELP-TIRFS                                        
077900     MOVE ZERO       TO TRAN-TIAAMMDD                                     
078000                        TRAN-TIHHMM                                       
078100                                                                          
078200     MOVE HELP-TIRFS-DAT     TO TMP1-YYMMDD                               
078300     MOVE SPAR-PREL-TATDAT   TO TMP2-YYMMDD                               
078400     PERFORM WY2000P1                                                     
078500     IF TMP1-YYMMDD < TMP2-YYMMDD                                         
078600       MOVE K-KDSVAR-RFSDAT-FEL-2 TO TRAN-KDSVAR                          
078700     ELSE                                                                 
078800       IF HELP-TIRFS-DAT = SPAR-PREL-TATDAT AND                           
078900          HELP-TIRFS-TID < SPAR-PREL-TATTID                               
079000         MOVE K-KDSVAR-RFSTID-FEL-3 TO TRAN-KDSVAR                        
079100       END-IF                                                             
079200     END-IF                                                               
079300     .                                                                    
079400     EJECT                                                                
079500 I-KOLLA-OM-TRPAVT-SKALL-SATTAS SECTION.                                  
079600                                                                          
079700     MOVE JA TO ALLT-SW                                                   
079800                                                                          
079900     IF (TRAN-KDTRPKAT     = 'B' OR 'C' ) AND                             
080000        (TRAN-KDORDKL      = +0 OR +1 )                                   
080100       MOVE NEJ TO ALLT-SW                                                
080200     ELSE                                                                 
080300       IF (TRAN-KDTRPKAT = 'B' OR 'C') AND                                
080400           TRAN-TIRFS    = ZERO                                           
080500         MOVE NEJ TO ALLT-SW                                              
080600       ELSE                                                               
080700         IF TRAN-KDTPOTYP > ZERO                                          
080800           MOVE NEJ TO ALLT-SW                                            
080900         ELSE                                                             
081000           IF TRAN-FLORDSPE = JA  OR                                      
081100              TRAN-FLOVRLEV = JA                                          
081200             MOVE NEJ TO ALLT-SW                                          
081300           END-IF                                                         
081400         END-IF                                                           
081500       END-IF                                                             
081600     END-IF                                                               
081700     .                                                                    
081800     EJECT                                                                
081900 S02-TA-FRAM-NAESTA-ARBVECKA SECTION.                                     
082000                                                                          
082100     MOVE 'S02-TA-FRAM-NAES.'  TO WS-SECTION-NAME                         
082200     MOVE NEJ TO SW-NAESTA-ARBVECKA-FUNNEN                                
082300                                                                          
082400     PERFORM UNTIL SW-NAESTA-ARBVECKA-FUNNEN = JA                         
082500       MOVE     'YYMMDD'                   TO DAYS-KDDATFMT1              
082600       MOVE     'YYWWD'                    TO DAYS-KDDATFMT2              
082700       MOVE     WORK-TIAAMMDD-NEXT-WEEK    TO DAYS-TIDATE1                
082800       MOVE     SPACE                      TO DAYS-TIDATE2                
082900                                              DAYS-IDCALEND               
083000       CALL     WZ20DAYS USING DAYS-WZ20DAYS                              
083100                                                                          
083200       MOVE     DAYS-TIDATE2(1:5)          TO HELP-TITRPAVG               
083300                                                                          
083400       IF SPAR-TITRPAVG-D NOT < HELP-TITRPAVG-D                           
083500                                                                          
083600         MOVE SPAR-TITRPAVG-D              TO    HELP-TITRPAVG-D          
083700                                                                          
083800         MOVE 'YYWWD'                      TO DAYS-KDDATFMT1              
083900         MOVE 'YYMMDD'                     TO DAYS-KDDATFMT2              
084000         MOVE HELP-TITRPAVG-AAVVD          TO DAYS-TIDATE1                
084100         MOVE SPACE                        TO DAYS-TIDATE2                
084200                                              DAYS-IDCALEND               
084300         CALL WZ20DAYS USING DAYS-WZ20DAYS                                
084400                                                                          
084500         MOVE DAYS-TIDATE2(1:6)            TO WORK-TIAAMMDD-FOM           
084600                                              WORK-TIAAMMDD-TOM           
084700                                                                          
084800         PERFORM S11-CALL-WORKDAY                                         
084900         IF WORK-KVWORKD           = K-ARBETSDAG                          
085000           MOVE JA TO SW-NAESTA-ARBVECKA-FUNNEN                           
085100         END-IF                                                           
085200       ELSE                                                               
085300                                                                          
085400         MOVE    WORK-TIAAMMDD-NEXT-WEEK   TO    WORK-TIAAMMDD-FOM        
085500                                                 WORK-TIAAMMDD-TOM        
085600         PERFORM S11-CALL-WORKDAY                                         
085700       END-IF                                                             
085800     END-PERFORM                                                          
085900     .                                                                    
086000     EJECT                                                                
086100 S03-TA-FRAM-NAESTA-AARSTRP SECTION.                                      
086200                                                                          
086300     MOVE 'S03-TA-FRAM-NAES.'  TO WS-SECTION-NAME                         
086400     MOVE NEJ                 TO SW-GODK-AARSTRP-FUNNEN                   
086500                                                                          
086600     ADD    +1                TO HELP-TITRPAVG-AA                         
086700     MOVE 'YYWWD'             TO DAYS-KDDATFMT1                           
086800     MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                           
086900     MOVE HELP-TITRPAVG-AAVVD TO DAYS-TIDATE1                             
087000     MOVE SPACE               TO DAYS-TIDATE2                             
087100                                 DAYS-IDCALEND                            
087200     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
087300                                                                          
087400     MOVE DAYS-TIDATE2(1:6)   TO WORK-TIAAMMDD-FOM                        
087500                                 WORK-TIAAMMDD-TOM                        
087600     PERFORM S11-CALL-WORKDAY                                             
087700                                                                          
087800     IF WORK-KVWORKD = K-ARBETSDAG                                        
087900        MOVE JA               TO    SW-GODK-AARSTRP-FUNNEN                
088000     END-IF                                                               
088100     .                                                                    
088200     EJECT                                                                
088300 S04-BESTAM-PREL-TAT-FRAN-TIRFS SECTION.                                  
088400                                                                          
088500     MOVE 'S04-BESTAM-PREL..'  TO WS-SECTION-NAME                         
088600     MOVE    NEJ            TO SW-PREL-TAT-SATT                           
088700     MOVE    HELP-TIRFS-DAT TO SPAR-PREL-TATDAT                           
088800                               WORK-TIAAMMDD-FOM                          
088900                               WORK-TIAAMMDD-TOM                          
089010     MOVE    HELP-TIRFS-TID TO SPAR-PREL-TATTID                           
089100     PERFORM S11-CALL-WORKDAY                                             
089200                                                                          
089300     PERFORM UNTIL SW-PREL-TAT-SATT = JA                                  
089400                                                                          
089500       IF WORK-KVWORKD = K-ARBETSDAG                                      
089600         MOVE WORK-TIAAMMDD-FOM TO SPAR-PREL-TATDAT                       
089700         MOVE JA                TO SW-PREL-TAT-SATT                       
089800       ELSE                                                               
089900         MOVE    WORK-TIAAMMDD-NEXT-WORKDAY TO WORK-TIAAMMDD-FOM          
090000                                               WORK-TIAAMMDD-TOM          
090100         PERFORM S11-CALL-WORKDAY                                         
090200       END-IF                                                             
090300     END-PERFORM                                                          
090400                                                                          
090500     PERFORM S06-SPARA-FALT-VID-PTAT-TIDPKT                               
090600     .                                                                    
090700     EJECT                                                                
090800 S06-SPARA-FALT-VID-PTAT-TIDPKT SECTION.                                  
090900                                                                          
091000     MOVE WORK-TIAAMMDD-NEXT-WORKDAY TO    SPAR-NAESTA-ARBDAG             
091100     MOVE WORK-TIAAMMDD-NEXT-WEEK    TO    SPAR-NAESTA-VECKA              
091200                                                                          
091300     MOVE 'YYMMDD'                   TO    DAYS-KDDATFMT1                 
091400     MOVE 'YYWWD'                    TO    DAYS-KDDATFMT2                 
091500     MOVE SPAR-PREL-TATDAT           TO    DAYS-TIDATE1                   
091600     MOVE SPACE                      TO    DAYS-TIDATE2                   
091700                                           DAYS-IDCALEND                  
091800     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
091900                                                                          
092000     MOVE DAYS-TIDATE2(1:5)          TO    SPAR-PREL-TATDAT-AAVVD         
092100     .                                                                    
092200     EJECT                                                                
092300 S11-CALL-WORKDAY SECTION.                                                
092400                                                                          
092500     MOVE WS-IDDC               TO WORK-IDDC                              
092600     MOVE +001                  TO WORK-KDCALL                            
092700     CALL WORKDAY               USING WORK-KDCALL                         
092800                                      WORK-DATE-AREA                      
092900                                      WORK-KDSVAR                         
093000     IF WORK-KDSVAR-FEL                                                   
093100*       MOVE 'SECT S11-, DATUM SAKNAS I WORKDAY'                          
093200*                               TO    FELTEXT-VID-CALL-ABEND              
093300        MOVE WS-SECTION-NAME    TO    FELTEXT-VID-CALL-ABEND              
093400        CALL ABEND              USING RKOD-ABEND-MED-DUMP                 
093500     END-IF                                                               
093600     .                                                                    
093700     EJECT                                                                
093800 S13-CALL-WORKDAY-LDC SECTION.                                            
093900                                                                          
094000     MOVE WS-IDDC               TO WORK-IDDC                              
094100     MOVE +002                  TO WORK-KDCALL                            
094200     MOVE +002                  TO WORK-KVWORKD                           
094300     MOVE SPAR-DAGENS-DATUM     TO WORK-TIAAMMDD-FOM                      
094400     CALL WORKDAY               USING WORK-KDCALL                         
094500                                      WORK-DATE-AREA                      
094600                                      WORK-KDSVAR                         
094700     IF WORK-KDSVAR-FEL                                                   
094800        MOVE 'SECT S13-, DATUM SAKNAS I WORKDAY'                          
094900                                TO    FELTEXT-VID-CALL-ABEND              
095000        CALL ABEND              USING RKOD-ABEND-MED-DUMP                 
095100     END-IF                                                               
095200     .                                                                    
095300     EJECT                                                                
095400 IMS-GU-XXKB01 SECTION.                                                   
095500                                                                          
095600     STRING  'WLXXKB01(WDGXKEY  =' W-WDGXKEY-4433-X ')'                   
095700             DELIMITED BY SIZE INTO SSA1                                  
095800     MOVE    '  '               TO GODK-STATUSKODER                       
095900     CALL    CBLTDLI USING GU XXKB-PCB DLI-IO-AREA SSA1                   
096000     MOVE    XXKB-STATUS-CODE   TO STATUS-WS                              
096100     PERFORM IMS-STATUSKONTROLL                                           
096200     .                                                                    
096300     SKIP2                                                                
096400 IMS-GNP-XXKB11 SECTION.                                                  
096500                                                                          
096600     STRING  'WLXXKB11(WDGXKEY >=' W-WDGXKEY-4434-MIN-X                   
096700                     '&WDGXKEY <=' W-WDGXKEY-4434-MAX-X ')'               
096800             DELIMITED BY SIZE INTO SSA1                                  
096900     MOVE    '  GE'             TO GODK-STATUSKODER                       
097000     CALL    CBLTDLI USING GNP XXKB-PCB DLI-IO-AREA SSA1                  
097100     MOVE    XXKB-STATUS-CODE   TO STATUS-WS                              
097200     PERFORM IMS-STATUSKONTROLL                                           
097300     .                                                                    
097400     EJECT                                                                
097500 IMS-STATUSKONTROLL SECTION.                                              
097600                                                                          
097700     SET    STATUS-IX TO +1                                               
097800     SEARCH GODK-STATUS                                                   
097900       AT END                                                             
098000         CALL FELLOG                                                      
098100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
098200         CONTINUE                                                         
098300     END-SEARCH                                                           
098400     .                                                                    
098500     EJECT                                                                
098600*    -COPY WY2000P1                                                       
098700     EJECT                                                                
098800*    -COPY WY2000P9                                                       
