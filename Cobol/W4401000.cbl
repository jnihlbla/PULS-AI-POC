000100 ID DIVISION.                                                             
000300 PROGRAM-ID.             W4401000.                                        
000400 AUTHOR.                 BO SVENSSON.                                     
000500 DATE-WRITTEN.           JAN 1997.                                        
000510 DATE-COMPILED.                                                           
000600                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*      SÖKER FRAM RESTORDER-TÄCKNINGS DC 51       MHA GENOM-              
001100*      LÄSNING AV DC 51'S LAGERBAND.                                      
001200*                                                                         
002200*      HUVUDLOGIK:                                                        
002300*        OM EN ARTIKEL                                                    
002400*              OM  KVROS > 0 OCH DISPONIBELT > 0                          
002600*        ELLER ERSÄTTNINGSKOD > 10                                        
002700*            SÅ SKAPAS EN HTR TILL RO-TÄCKNING (WL4505)                   
002800*            MED KDTAKORS 21, RESP FILEN W44033 TILL RO-ERS.              
003100*                                                                         
003200*      PROGRAMMET KÖRS SOM BMP.                                           
003300*      BMP-STYR-HTR UPPDATERAS MED CHKP-RÄKNARE MM.                       
003400*      VID ÅTERSTART LÄSES INFILEN FRAM TILL SENASTE CHKP-LÄGE.           
003500*      CHKP TAS VID BRYTNING IDARTNR OM MAX ANTAL UPPDAT (100 ST)         
003600*      PER CHKP UPPNÅTTS.                                                 
003800*                                                                         
004300*      HTR 4551 WL4551/WDR4 UPPDATERAS. (BMP-STYR-HTR)                    
004400*      HTR 4505 WL4505/WDR4 UPPDATERAS. (RO-TÄCKNINGS-HTR).               
004500*                                                                         
004600*    ABENDKODER:                                                          
004700*      U0016 - INFIL OCH CHKP-HÄNDELSETRANS STÄMMER EJ ÖVERENS.           
004800*                                                                         
005000     EJECT                                                                
005100 ENVIRONMENT DIVISION.                                                    
005200                                                                          
005300 INPUT-OUTPUT SECTION.                                                    
005400                                                                          
005500 FILE-CONTROL.                                                            
005600     SKIP2                                                                
005700*    ---- INFIL: LAGERBAND DC 51                                          
005800     SELECT  LBFIL         ASSIGN  W44033D1.                              
005900     SKIP2                                                                
006000*    ---- UTFIL: ARTIKLAR FÖR ERSÄTTNING I RO                             
006100     SELECT  W44031        ASSIGN  W44033D2.                              
006200     EJECT                                                                
006300 DATA DIVISION.                                                           
006400                                                                          
006500 FILE SECTION.                                                            
006600     SKIP3                                                                
006700 FD  LBFIL                                                                
006800     LABEL RECORD STANDARD                                                
006900     RECORDING  F                                                         
007000     BLOCK CONTAINS 0.                                                    
007100                                                                          
007200*01  LBPOST -COPY W01184        -L.                                       
007300     SKIP3                                                                
007400 FD  W44031                                                               
007500     LABEL RECORD STANDARD                                                
007600     RECORDING  F                                                         
007700     BLOCK CONTAINS 0.                                                    
007800                                                                          
007900*01  UTPOST -COPY W440004 -PRE W44031-  -L.                               
008000     EJECT                                                                
008100 WORKING-STORAGE SECTION.                                                 
008200                                                                          
008201                                                                          
008210*    -- CHECKED BY WY2000                                                 
008300 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W4403300'.                
008400 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
008500                                                                          
008600 77  CHKP-ID                 PIC X(8)    VALUE 'W44033  '.                
008700 77  MSG-IO-AREA-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
008800 77  MSG-IO-AREA             PIC X(32)   VALUE SPACE.                     
008900 77  CHKP-AREA-1-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
009000 77  CHKP-AREA-1             PIC X(32)   VALUE SPACE.                     
009100                                                                          
009200 77  LBFIL-EOF               PIC X       VALUE 'N'.                       
009300     EJECT                                                                
009400 01      FILLER              PIC X(16)   VALUE                            
009500                                         'W***************'.              
009600 01      W.                                                               
009700*                                                                         
009800  02     W-ANT-POST-LB       PIC S9(7)   VALUE ZERO  COMP-3.              
009900  02     W-ANT-UPD-CHKP      PIC S9(7)   VALUE ZERO  COMP-3.              
010000  02     W-DISP              PIC S9(7)   VALUE ZERO  COMP-3.              
010100                                                                          
010200  02     W-AAMMDD            PIC 9(6).                                    
010300  02     FILLER              REDEFINES W-AAMMDD.                          
010400   03    W-AAMMDD-AA         PIC 9(2).                                    
010500   03    W-AAMMDD-MM         PIC 9(2).                                    
010600   03    W-AAMMDD-DD         PIC 9(2).                                    
010700                                                                          
010800  02     W-AAVVD             PIC 9(5).                                    
010900  02     FILLER              REDEFINES W-AAVVD.                           
011000   03    W-AAVVD-AA          PIC 9(2).                                    
011100   03    W-AAVVD-VV          PIC 9(2).                                    
011200   03    W-AAVVD-D           PIC 9(1).                                    
011300     SKIP1                                                                
011400*    INFO FRÅN SIST KONSUMERAD LB-POST FÖRE CHKP                          
011500  02     W-CHKPID.                                                        
011600   03    W-CHKPID-KVPOST     PIC S9(7)              COMP-3.               
011700   03    W-CHKPID-IDARTNR    PIC S9(9)              COMP-3.               
011800     SKIP1                                                                
011900*    TIDS-STÄMPEL FÖR CHKP-HTR                                            
012000  02     W-TS.                                                            
012100   03    W-TS-TIAAMMDD       PIC 9(6).                                    
012200   03    W-TS-TIKLOCK        PIC 9(8).                                    
012300     EJECT                                                                
012400 01      FILLER              PIC X(16)   VALUE                            
012500                                         'ARTW************'.              
012600*        SPARAD INFO FRÅN AKTUELL ARTIKEL.                                
012700 01      ARTW.                                                            
012800*                                                                         
012900   03    ARTW-IDARTNR        PIC S9(9)   VALUE ZERO  COMP-3.              
013000   03    ARTW-KVDISP         PIC S9(7)               COMP-3.              
013100   03    ARTW-KVROS          PIC S9(7)               COMP-3.              
013110   03    ARTW-KVAKS          PIC S9(7)               COMP-3.              
013200     SKIP3                                                                
013300 01      FILLER              PIC X(16)   VALUE                            
013400                                         'K-KONSTANTER****'.              
013500 01      K-KONSTANTER.                                                    
013600*                                                                         
013700  02     K-MAX-UPD-CHKP      PIC S9(5)   VALUE +100  COMP-3.              
013800  02     K-KDTAKORS          PIC S9(3)   VALUE +21   COMP-3.              
013900  02     JA                  PIC X(1)    VALUE 'J'.                       
014000  02     NEJ                 PIC X(1)    VALUE 'N'.                       
014100     SKIP3                                                                
014200 01      FILLER              PIC X(16)   VALUE                            
014300                                         'SW-SWITCHAR*****'.              
014400 01      SW-SWITCHAR.                                                     
014500*                                                                         
014600  02     SW-BMP              PIC X(1).                                    
014700   88    BMP                             VALUE 'J'.                       
014800  02     SW-SPARR-OK         PIC X(1).                                    
014900     EJECT                                                                
014910*      --- VALID IDDC CODES                                               
014920*                                                                         
014930*01    -COPY WWDCKONS                                                     
014940       EJECT                                                              
015000 01      FILLER              PIC X(16)   VALUE                            
015100                                         'DYNAM-SUBPGM****'.              
015200 01      DYNAM-SUBPGM.                                                    
015300*                                                                         
015400  02     ABEND               PIC X(8)    VALUE 'ABEND   '.                
015500  02     FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
015600  02     POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
015700  02     CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
015800  02     DATKORT             PIC X(8)    VALUE 'DATKORT '.                
015900  02     VIMSREGT            PIC X(8)    VALUE 'VIMSREGT'.                
016000     EJECT                                                                
016100 01      FILLER              PIC X(16)   VALUE                            
016200                                         'LBW*************'.              
016300*01  POST   -COPY W01184       -PRE LBW-                                  
016400     EJECT                                                                
016500 01      FILLER              PIC X(16)   VALUE                            
016600                                         'UTFIL***********'.              
016700*01  AREA   -COPY W440004      -PRE UT-                                   
016800     EJECT                                                                
016900*    ----  PARAMETRAR TILL ABEND                                          
017000     SKIP1                                                                
017100 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16 COMP SYNC.               
017200     EJECT                                                                
017300*    ----  PARAMETRAR TILL POSTSUM                                        
017400                                                                          
017500*01  -COPY W0005       -PRE POSTSUM-.                                     
017600     EJECT                                                                
017700*    ----  PARAMETRAR TILL DATUMKORT                                      
017800                                                                          
017900 01  DATUMKORT-ID            PIC X(6)   VALUE 'WDATUM'.                   
018000     SKIP3                                                                
018100*01  -COPY WDATKORT                                                       
018200     EJECT                                                                
018300*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
018400                                                                          
018500 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
018600     SKIP3                                                                
018700*    ---- STATUSKOD FRÅN IMS                                              
018800                                                                          
018900 01  STATUS-WS               PIC XX.                                      
019000     88  SEGMENT-FINNS                    VALUE '  '.                     
019100     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
019200     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
019300     88  IMS-EJ-OK                        VALUE 'XD'.                     
019400     SKIP3                                                                
019500 01  GODK-STATUSKODER.                                                    
019600   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
019700     SKIP3                                                                
019800 01  SSA1                    PIC X(64).                                   
019900 01  SSA2                    PIC X(32).                                   
020000     EJECT                                                                
020100*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
020200                                                                          
020300 01      FILLER              PIC X(16)   VALUE                            
020400                                         'NYCKAR-TILL-DLI*'.              
020500 01      NYCKLAR-TILL-DLI.                                                
020600*                                                                         
020700  02     W-IDHTYP-4551-X.                                                 
020800   03    FILLER              PIC X(4)    VALUE '4551'.                    
020900   03    FILLER              PIC X(26)   VALUE LOW-VALUE.                 
021000*                                                                         
021100  02     W-IDHTYP-4505-X.                                                 
021200   03    FILLER              PIC X(4)    VALUE '4505'.                    
021300   03    IDDC-4505           PIC X(2)    VALUE '51'.                      
021310   03    FILLER              PIC X(24)   VALUE LOW-VALUE.                 
021320                                                                          
021330  02     W-IDARTNR-K6-X.                                                  
021340   03    W-IDARTNR-K6        PIC S9(9)   COMP-3 VALUE ZERO.               
021350                                                                          
021400     EJECT                                                                
021500*01  -COPY W0003                                                          
021600     EJECT                                                                
021700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
021800     SKIP1                                                                
021900 01  DLI-IO-AREA.                                                         
022000  03 IO-AREA                 PIC X(300).                                  
022100     SKIP1                                                                
022200*03  WL455111 -COPY WDGX4552    -PRE 4551-     -RED IO-AREA               
022300     EJECT                                                                
022400*03  WL450511 -COPY WDGX4506    -PRE 4505-     -RED IO-AREA               
022500     EJECT                                                                
022510                                                                          
022520 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTC11'.             
022530 01  DLI-IO-ARTC11.                                                       
022540*  03  WLARTC11 -COPY WDK611                                              
022550     EJECT                                                                
022600 LINKAGE SECTION.                                                         
022700     SKIP2                                                                
022800*01  -COPY W0009      -PRE  MSG-                                          
022900     EJECT                                                                
023000*01  -COPY W0008      -PRE  4551-                                         
023100       05  FILLER                PIC X.                                   
023200     SKIP2                                                                
023300*01  -COPY W0008      -PRE  4505-                                         
023400       05  FILLER                PIC X.                                   
023410     SKIP2                                                                
023420*01  -COPY W0008      -PRE  ARTC-                                         
023430       05  FILLER                PIC X.                                   
023500     EJECT                                                                
023600 PROCEDURE DIVISION  USING  MSG-PCB 4551-PCB 4505-PCB ARTC-PCB.           
023700     ENTRY 'DLITCBL' USING  MSG-PCB 4551-PCB 4505-PCB ARTC-PCB.           
023800     SKIP2                                                                
023900     PERFORM A-INIT                                                       
024000                                                                          
024100     PERFORM S01-LAS-LBFIL                                                
024200                                                                          
024300     PERFORM UNTIL (LBFIL-EOF = JA)                                       
024400                                                                          
024500       PERFORM B-INIT-ART                                                 
024600                                                                          
024700       PERFORM UNTIL (LBFIL-EOF = JA                                      
024800                  OR  LBW-SLAG-IDARTNR NOT = ARTW-IDARTNR)                
025100                                                                          
025200         PERFORM D-UTVARD-LBPOST                                          
025300         PERFORM S02-SPARA-CHKPID                                         
025400         PERFORM S01-LAS-LBFIL                                            
025500       END-PERFORM                                                        
025600                                                                          
025700       IF ARTW-KVROS  > ZERO                                              
025710         IF  CLAG-KDERS = 22                                              
025720         OR  CLAG-KDERS = 23                                              
025721             MOVE ARTW-IDARTNR     TO UT-IDARTNR                          
025722             MOVE WC-NDC-CA        TO UT-IDDC                             
025724             PERFORM S03-SKRIV-W44031                                     
025730         ELSE                                                             
025731             IF ARTW-KVDISP > ZERO                                        
025732*        * ARTIKELN HAR BÅDE RESTORDER OCH DISPONIBELT SALDO.             
025733                PERFORM E-GEN-TACKN-HTR                                   
025734             ELSE                                                         
025735               IF  ARTW-KVAKS = 0                                         
025736               AND CLAG-KDERS > +10                                       
025738                  MOVE ARTW-IDARTNR     TO UT-IDARTNR                     
025739                  MOVE WC-NDC-CA        TO UT-IDDC                        
025741                  PERFORM S03-SKRIV-W44031                                
025742               END-IF                                                     
025743             END-IF                                                       
025750         END-IF                                                           
026110       END-IF                                                             
026200                                                                          
026300*---------------------------- VID BRYTNING IDARTNR,                       
026400*                             OM ANTAL UPPD. UPPNÅTT MAX-GRÄNS:           
026500*                             I DET FALL LB-FILEN EJ ÄR SLUT;             
026600*                              TAS CHECKPOINT.                            
026700*                             VID EOF LB-FIL;                             
026800*                              TAS INGEN CHECKPOINT, EFTERSOM             
026900*                              NORMALT PGM-AVSLUT SNART SKALL SKE.        
027000       IF  BMP                                                            
027100       AND W-ANT-UPD-CHKP >= K-MAX-UPD-CHKP                               
027200         IF  LBFIL-EOF = NEJ                                              
027300           PERFORM F-TAG-CHECKPOINT                                       
027400         END-IF                                                           
027500       END-IF                                                             
027600                                                                          
027700     END-PERFORM                                                          
027800                                                                          
027900     PERFORM Z-FINIT                                                      
028000     MOVE ZERO TO RETURN-CODE                                             
028100     GOBACK                                                               
028200     .                                                                    
028300     EJECT                                                                
028400 A-INIT SECTION.                                                          
028500                                                                          
028600     CALL VIMSREGT                                                        
028700     IF  RETURN-CODE = +8                                                 
028800       MOVE JA               TO SW-BMP                                    
028900     ELSE                                                                 
029000       MOVE NEJ              TO SW-BMP                                    
029100     END-IF                                                               
029200     SKIP1                                                                
029300     OPEN INPUT  LBFIL                                                    
029400          OUTPUT W44031                                                   
029500     SKIP1                                                                
029600     MOVE NEJ                TO LBFIL-EOF                                 
029700     MOVE ZERO               TO W-ANT-POST-LB                             
029800     MOVE ZERO               TO ARTW-IDARTNR                              
029900     MOVE ZERO               TO W-CHKPID-KVPOST                           
030000     MOVE ZERO               TO W-CHKPID-IDARTNR                          
030100                                                                          
030200     IF  BMP                                                              
030300       PERFORM AA-INIT-BMP                                                
030400     END-IF                                                               
030500                                                                          
030600     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
030700                                                                          
030800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
030900     MOVE D-AAR    TO W-AAMMDD-AA                                         
031000     MOVE D-MAANAD TO W-AAMMDD-MM                                         
031100     MOVE D-DAG    TO W-AAMMDD-DD                                         
031200     MOVE D-AAR    TO W-AAVVD-AA                                          
031300     MOVE D-VECKA  TO W-AAVVD-VV                                          
031400     MOVE D-DAGNR  TO W-AAVVD-D                                           
031500     .                                                                    
031600     EJECT                                                                
031700 AA-INIT-BMP SECTION.                                                     
031800                                                                          
031900     PERFORM IMS-RESTART                                                  
032000                                                                          
032100     MOVE ZERO               TO W-ANT-UPD-CHKP                            
032200                                                                          
032300     PERFORM IMS-GHU-4551-4552                                            
032400                                                                          
032500     IF SEGMENT-FINNS                                                     
032600                                                                          
032700       IF  4551-4552-KVPOST > ZERO                                        
032800                                                                          
032900*--------------------------- LBFIL LÄSES                                  
033000*                            FRAM TILL CHECKPOINT-LÄGE                    
033100                                                                          
033200         PERFORM S01-LAS-LBFIL                                            
033300                                                                          
033400         PERFORM UNTIL (LBFIL-EOF = JA                                    
033500                    OR  W-ANT-POST-LB >= 4551-4552-KVPOST)                
033600           PERFORM S01-LAS-LBFIL                                          
033700         END-PERFORM                                                      
033800                                                                          
033900         IF  LBFIL-EOF       = JA                                         
034000         OR  W-ANT-POST-LB   NOT = 4551-4552-KVPOST                       
034100         OR  LBW-SLAG-IDARTNR     NOT = 4551-4552-IDARTNR                 
034200           DISPLAY 'W4403300: FEL I ÅTERSTARTEN, LB-FIL'                  
034300           CALL ABEND USING  RKOD-ABEND-UTAN-DUMP                         
034400         END-IF                                                           
034500                                                                          
034600*------- NU ÄR DEN LB-POST INLÄST, SOM VID FÖREGÅENDE EXEKVERING          
034700*        "KONSUMERADES" NÄRMAST FÖRE SISTA CHECKPOINT.                    
034800                                                                          
034900         PERFORM S02-SPARA-CHKPID                                         
035000       END-IF                                                             
035100     END-IF                                                               
035200     .                                                                    
035300     EJECT                                                                
035400 B-INIT-ART SECTION.                                                      
035500                                                                          
035600     MOVE LBW-SLAG-IDARTNR TO ARTW-IDARTNR                                
035700                                                                          
035800     MOVE ZERO        TO ARTW-KVDISP                                      
035900     MOVE ZERO        TO ARTW-KVROS                                       
035910     MOVE ZERO        TO ARTW-KVAKS                                       
036000     .                                                                    
037200     EJECT                                                                
037300 D-UTVARD-LBPOST SECTION.                                                 
037400                                                                          
037420                                                                          
037500     ADD  LBW-SLAG-KVROS-DAG                                              
037510          LBW-SLAG-KVROS-BULK  GIVING ARTW-KVROS                          
037600                                                                          
037610     ADD  LBW-SLAG-KVAKS-SDC                                              
037620          LBW-SLAG-KVAKS-PAV   GIVING ARTW-KVAKS                          
037621                                                                          
037622     IF  ARTW-KVROS > 0                                                   
037623         MOVE ARTW-IDARTNR TO W-IDARTNR-K6                                
037624         PERFORM IMS-GU-ARTC11                                            
037630                                                                          
037700         MOVE JA                 TO SW-SPARR-OK                           
037800                                                                          
037900         IF LBW-SLAG-KDLEVSP > 0   OR                                     
038000            LBW-SLAG-FLORDSP = JA                                         
038100           MOVE NEJ              TO SW-SPARR-OK                           
038200         END-IF                                                           
038300                                                                          
038400         IF  SW-SPARR-OK = JA                                             
038500           IF LBW-SLAG-KVRESS < ZERO                                      
038600             MOVE ZERO TO LBW-SLAG-KVRESS                                 
038700           END-IF                                                         
038800           COMPUTE W-DISP        =  LBW-SLAG-KVLS                         
038900                                 -  LBW-SLAG-KVOKS-DAG                    
039000                                 -  LBW-SLAG-KVSPARR-KVAL                 
039100                                 -  LBW-SLAG-KVUTRS                       
039110                                 -  LBW-SLAG-KVRESS                       
039200                                                                          
039300           IF  W-DISP > ZERO                                              
039400             IF  CLAG-PRARTSTD > ZERO                                     
039500               MOVE W-DISP       TO ARTW-KVDISP                           
039600             END-IF                                                       
039700           END-IF                                                         
039800         ELSE                                                             
039801            MOVE ZERO            TO W-DISP                                
039810         END-IF                                                           
039820     END-IF                                                               
039900     .                                                                    
040000     EJECT                                                                
040100 E-GEN-TACKN-HTR SECTION.                                                 
040200                                                                          
040210     MOVE WC-NDC-CA          TO IDDC-4505                                 
040300     MOVE SPACE              TO 4505-4506-WDGX4506                        
040400     MOVE ARTW-IDARTNR       TO 4505-4506-IDARTNR                         
040500     MOVE K-KDTAKORS         TO 4505-4506-KDTAKORS                        
040600     MOVE ZERO               TO 4505-4506-KVANTMOT                        
040700     PERFORM IMS-ISRT-4505-4506                                           
040800                                                                          
040900     IF  BMP                                                              
041000       ADD +1                TO W-ANT-UPD-CHKP                            
041100     END-IF                                                               
041200     .                                                                    
041300     EJECT                                                                
041400 F-TAG-CHECKPOINT SECTION.                                                
041500                                                                          
041600     PERFORM IMS-GHU-4551-4552                                            
041700                                                                          
041800     MOVE SPACE              TO   4551-4552-WDGX4552                      
041900     MOVE '1'                TO   4551-4552-KDSEGKEY                      
042000                                                                          
042100     MOVE   W-CHKPID-KVPOST  TO   4551-4552-KVPOST                        
042200     MOVE   W-CHKPID-IDARTNR TO   4551-4552-IDARTNR                       
042300                                                                          
042400     ACCEPT W-TS-TIAAMMDD    FROM DATE                                    
042500     MOVE   W-TS-TIAAMMDD    TO   4551-4552-TIUPPDAT                      
042600     ACCEPT W-TS-TIKLOCK     FROM TIME                                    
042700     MOVE   W-TS-TIKLOCK     TO   4551-4552-TIUPPTID                      
042800                                                                          
042900     IF  SEGMENT-FINNS                                                    
043000       PERFORM IMS-REPL-4551                                              
043100     ELSE                                                                 
043200       PERFORM IMS-ISRT-4551-4552                                         
043300     END-IF                                                               
043400                                                                          
043500     MOVE CHKP-ID            TO MSG-IO-AREA                               
043600     PERFORM IMS-CHECKPOINT                                               
043700                                                                          
043800     MOVE ZERO               TO W-ANT-UPD-CHKP                            
043900     .                                                                    
044000     EJECT                                                                
044100 Z-FINIT SECTION.                                                         
044200                                                                          
044300     CLOSE  LBFIL                                                         
044400            W44031                                                        
044500                                                                          
044600     IF  BMP                                                              
044700       PERFORM ZA-FINIT-BMP                                               
044800     END-IF                                                               
044900                                                                          
045000     MOVE 'S' TO POSTSUM-OPKOD                                            
045100     CALL POSTSUM USING POSTSUM-PARM                                      
045200     .                                                                    
045300     EJECT                                                                
045400 ZA-FINIT-BMP SECTION.                                                    
045500*                                                                         
045600*    HTR FÖR CHKP-INFO TAS BORT                                           
045700*                                                                         
045800     PERFORM IMS-GHU-4551-4552                                            
045900                                                                          
046000     IF  SEGMENT-FINNS                                                    
046100       PERFORM IMS-DLET-4551                                              
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500 S01-LAS-LBFIL SECTION.                                                   
046600                                                                          
046700     READ LBFIL INTO LBW-POST                                             
046800       AT  END                                                            
046900         MOVE JA               TO LBFIL-EOF                               
047000     END-READ                                                             
047100                                                                          
047200     IF  LBFIL-EOF = NEJ                                                  
047300       MOVE 'LBFIL'            TO POSTSUM-FDNAMN                          
047400       MOVE 'W44033D1'         TO POSTSUM-DDNAMN2                         
047500       MOVE 'LB'               TO POSTSUM-TRANSTYP                        
047600       CALL POSTSUM USING POSTSUM-PARM                                    
047700       ADD +1                  TO W-ANT-POST-LB                           
047800     END-IF                                                               
047900     .                                                                    
048000     EJECT                                                                
048100 S02-SPARA-CHKPID SECTION.                                                
048200*                                                                         
048300*    HÄR SPARAS INFO FRÅN DEN SENAST KONSUMERADE LB-POSTEN,               
048400*    FÖR ATT KUNNA SPARA RÄTT INFO VID EV. CHKP.                          
048500*    DETTA GÖRS FÖRE LÄSNING AV NÄSTA LB-POST.                            
048600*                                                                         
048700     MOVE W-ANT-POST-LB        TO W-CHKPID-KVPOST                         
048800     MOVE LBW-SLAG-IDARTNR     TO W-CHKPID-IDARTNR                        
048900     .                                                                    
049000     EJECT                                                                
049100 S03-SKRIV-W44031 SECTION.                                                
049200                                                                          
049300     WRITE W44031-UTPOST FROM UT-AREA                                     
049400                                                                          
049500     MOVE 'W44031'           TO POSTSUM-FDNAMN                            
049600     MOVE 'W44033D2'         TO POSTSUM-DDNAMN2                           
049700     MOVE 'ERS-RO'           TO POSTSUM-TRANSTYP                          
049800     CALL POSTSUM USING POSTSUM-PARM                                      
049900     .                                                                    
050000     EJECT                                                                
050100*    ---- IMS SEKTIONER                                                   
050200                                                                          
050300 IMS-RESTART      SECTION.                                                
050400                                                                          
050500     MOVE SPACE TO MSG-IO-AREA                                            
050600     MOVE '  ' TO GODK-STATUSKODER                                        
050700     CALL CBLTDLI USING XRST                                              
050800                        MSG-PCB                                           
050900                        MSG-IO-AREA-LENGTH                                
051000                        MSG-IO-AREA                                       
051100                        CHKP-AREA-1-LENGTH                                
051200                        CHKP-AREA-1                                       
051300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
051400     PERFORM IMS-STATUSKONTROLL                                           
051500     .                                                                    
051600     SKIP3                                                                
051700 IMS-CHECKPOINT   SECTION.                                                
051800                                                                          
051900     MOVE SPACE TO MSG-IO-AREA                                            
052000     MOVE '  XD' TO GODK-STATUSKODER                                      
052100     CALL CBLTDLI USING CHKP                                              
052200                        MSG-PCB                                           
052300                        MSG-IO-AREA-LENGTH                                
052400                        MSG-IO-AREA                                       
052500                        CHKP-AREA-1-LENGTH                                
052600                        CHKP-AREA-1                                       
052700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
052800     PERFORM IMS-STATUSKONTROLL                                           
052900                                                                          
053000     IF  IMS-EJ-OK                                                        
053100       DISPLAY 'IMS-KONTROLLREGIONEN EJ TILLGÄNGLIG'                      
053200       CALL FELLOG                                                        
053300     END-IF                                                               
053400     .                                                                    
053500     EJECT                                                                
053600 IMS-GHU-4551-4552 SECTION.                                               
053700                                                                          
053800     STRING 'WL455101(WDGXKEY  =' W-IDHTYP-4551-X ')'                     
053900            DELIMITED BY SIZE INTO SSA1                                   
054000     STRING 'WL455111(KDSEGKEY =' '1' ')'                                 
054100            DELIMITED BY SIZE INTO SSA2                                   
054200     MOVE '  GE' TO GODK-STATUSKODER                                      
054300     CALL CBLTDLI USING GHU  4551-PCB DLI-IO-AREA SSA1 SSA2               
054400     MOVE 4551-STATUS-CODE TO STATUS-WS                                   
054500     PERFORM IMS-STATUSKONTROLL                                           
054600     .                                                                    
054700     SKIP3                                                                
054800 IMS-REPL-4551 SECTION.                                                   
054900                                                                          
055000     MOVE '  ' TO GODK-STATUSKODER                                        
055100     CALL CBLTDLI USING REPL 4551-PCB DLI-IO-AREA                         
055200     MOVE 4551-STATUS-CODE TO STATUS-WS                                   
055300     PERFORM IMS-STATUSKONTROLL                                           
055400     .                                                                    
055500     EJECT                                                                
055600 IMS-DLET-4551 SECTION.                                                   
055700                                                                          
055800     MOVE '  ' TO GODK-STATUSKODER                                        
055900     CALL CBLTDLI USING DLET 4551-PCB DLI-IO-AREA                         
056000     MOVE 4551-STATUS-CODE TO STATUS-WS                                   
056100     PERFORM IMS-STATUSKONTROLL                                           
056200     .                                                                    
056300     SKIP3                                                                
056400 IMS-ISRT-4551-4552 SECTION.                                              
056500                                                                          
056600     STRING 'WL455101(WDGXKEY  =' W-IDHTYP-4551-X ')'                     
056700            DELIMITED BY SIZE INTO SSA1                                   
056800     MOVE 'WL455111 ' TO SSA2                                             
056900     MOVE '  ' TO GODK-STATUSKODER                                        
057000     CALL CBLTDLI USING ISRT 4551-PCB DLI-IO-AREA SSA1 SSA2               
057100     MOVE 4551-STATUS-CODE TO STATUS-WS                                   
057200     PERFORM IMS-STATUSKONTROLL                                           
057300     .                                                                    
057400     EJECT                                                                
057500 IMS-ISRT-4505-4506 SECTION.                                              
057600                                                                          
057700     STRING 'WL450501(WDGXKEY  =' W-IDHTYP-4505-X ')'                     
057800            DELIMITED BY SIZE INTO SSA1                                   
057900     MOVE 'WL450511 ' TO SSA2                                             
058000     MOVE '  ' TO GODK-STATUSKODER                                        
058100     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-AREA SSA1 SSA2               
058200     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
058300     PERFORM IMS-STATUSKONTROLL                                           
058400     .                                                                    
058410     EJECT                                                                
058420 IMS-GU-ARTC11 SECTION.                                                   
058430                                                                          
058440     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-K6-X ')'                      
058450            DELIMITED BY SIZE INTO SSA1                                   
058451     MOVE 'WLARTC11 '           TO SSA2                                   
058460     MOVE '  '                  TO GODK-STATUSKODER                       
058470     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2              
058480     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
058490     PERFORM IMS-STATUSKONTROLL                                           
058491     .                                                                    
058500     EJECT                                                                
058600 IMS-STATUSKONTROLL SECTION.                                              
058700                                                                          
058800     SET STATUS-IX TO 1                                                   
058900     SEARCH GODK-STATUS                                                   
059000       AT  END                                                            
059100         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
059200           DELIMITED BY SIZE INTO FELTEXT                                 
059300         CALL FELLOG                                                      
059400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
059500     END-SEARCH                                                           
059600     .                                                                    
