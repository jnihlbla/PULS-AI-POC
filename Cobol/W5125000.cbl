000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5125000.                                                
000300 AUTHOR.         SARASWATHY.                                              
000400 DATE-WRITTEN.   17/04/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SB FOR WDL6                                                      
000900*        EXTRACT R30 FROM WDL6 IDDISTR 9111 AND 9211                      
001000 ENVIRONMENT DIVISION.                                                    
001100 INPUT-OUTPUT SECTION.                                                    
001200 FILE-CONTROL.                                                            
001300     SKIP2                                                                
001400*--- EXTRACT R30 FROM WDL6 IDDISTR 9111 AND 9211----                      
001500     SELECT UTFIL     ASSIGN TO W51250D1.                                 
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200     SKIP2                                                                
002300 FILE SECTION.                                                            
002400     SKIP3                                                                
002500 FD  UTFIL                                                                
002600     RECORDING       F                                                    
002700     BLOCK CONTAINS  0.                                                   
002800                                                                          
002900*01  POST -COPY W51250 -PRE  UT-  -L.                                     
003000     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W5125000'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004701 77  WS-IDDC-IX                  PIC 9(3)    VALUE ZERO.                  
004702 77  MAX-IDDC-IX                 PIC 9(3)    VALUE ZERO.                  
004703 01  WS-IDDC-TABELL.                                                      
004704     03 WS-VALID-IDDC  OCCURS 200.                                        
004705       05 TAB-IDDC            PIC X(2).                                   
004707       05 TAB-IDLEVNR         PIC X(5).                                   
004800                                                                          
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100                                                                          
006200 01  FELTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006500     EJECT                                                                
006600*01  -COPY WWDC99                                                         
006700     EJECT                                                                
006800*    --- PARAMETRAR TILL POSTSUM                                          
006900*                                                                         
007000*01  -COPY W0005   -PRE  POSTSUM-                                         
007100     EJECT                                                                
007200 01  UT-AREA-START               PIC X(24)   VALUE                        
007300                                 'UT-AREA-START  '.                       
007400*01  AREA -COPY W51250     -PRE UT-                                       
007500     EJECT                                                                
007600*                                                                         
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FINNS                       VALUE '  '.                  
008900     88  BASEN-SLUT                          VALUE 'GB'.                  
009000                                                                          
009100 01  GODK-STATUSKODER.                                                    
009200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     SKIP3                                                                
009400 01  SSA1                        PIC X(160).                              
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL6'.                        
010100 01  DLI-IO-WDL6.                                                         
010200     03 IO-AREA     PIC X(600) VALUE SPACE.                               
010300         03 DLI-IO-WDL601 REDEFINES IO-AREA.                              
010400*            05 -COPY WDL601                                              
010500     EJECT                                                                
010600         03 DLI-IO-WDL611 REDEFINES IO-AREA.                              
010700*            05 -COPY WDL611                                              
010710                                                                          
010720 01  FILLER         PIC X(16)   VALUE 'WDB601 AREA'.                      
010730 01   DLI-IO-AREA-B601.                                                   
010740*     03  -COPY WDB601                                                    
010750*                                                                         
010800 LINKAGE SECTION.                                                         
010900                                                                          
011000*01  -COPY W0008  -PRE WDL6-                                              
011100     05  FILLER                  PIC X.                                   
011110*01  -COPY W0008  -PRE WDB6-                                              
011120     05  FILLER                  PIC X.                                   
011130*                                                                         
011200     EJECT                                                                
011300 PROCEDURE DIVISION  USING WDL6-PCB,WDB6-PCB.                             
011400 MAIN SECTION.                                                            
011500     ENTRY 'DLITCBL' USING WDL6-PCB,WDB6-PCB.                             
011600                                                                          
011700     PERFORM A-INIT                                                       
011800                                                                          
011900     PERFORM IMS-GET-WDL6                                                 
012000     PERFORM UNTIL BASEN-SLUT                                             
012100       EVALUATE WDL6-SEG-NAME-FB                                          
012200         WHEN 'WDL601'                                                    
012300           MOVE ART-IDARTNR TO UT-IDARTNR                                 
012600         WHEN 'WDL611'                                                    
012700           PERFORM B-URVAL                                                
012800       END-EVALUATE                                                       
012900       PERFORM IMS-GET-WDL6                                               
013000     END-PERFORM                                                          
013100                                                                          
013200     PERFORM Z-FINIT                                                      
013300     MOVE ZERO TO RETURN-CODE                                             
013400     GOBACK                                                               
013500     .                                                                    
013600     EJECT                                                                
013700                                                                          
013800 A-INIT SECTION.                                                          
013900     OPEN OUTPUT UTFIL                                                    
014200     MOVE IDPGM    TO POSTSUM-PROGNAMN                                    
014210     INITIALIZE WS-IDDC-TABELL                                            
014220     MOVE +1 TO WS-IDDC-IX                                                
014230                MAX-IDDC-IX                                               
014240     PERFORM IMS-GN-WDB601                                                
014250     PERFORM UNTIL BASEN-SLUT                                             
014260        MOVE DCS-IDDC         TO TAB-IDDC(WS-IDDC-IX)                     
014280        MOVE DCS-IDLEVNR-DC   TO TAB-IDLEVNR(WS-IDDC-IX)                  
014290        ADD +1 TO WS-IDDC-IX                                              
014291                  MAX-IDDC-IX                                             
014292        IF WS-IDDC-IX > 200                                               
014293           MOVE 'DC-TABELLEN FULL' TO FELTEXT                             
014294           CALL FELLOG                                                    
014295        END-IF                                                            
014296        PERFORM IMS-GN-WDB601                                             
014297     END-PERFORM                                                          
014300     .                                                                    
014400     EJECT                                                                
014500                                                                          
014600 B-URVAL SECTION.                                                         
014700     IF INL-IDPTYP = 'R30'                                                
014800       MOVE INL-IDDC          TO WS-IDDC                                  
014900       IF CDC-SE OR LDC-SE                                                
015000       OR NDC-JP-6A OR NDC-JP-61                                          
015010       OR NDC-AU                                                          
015100         IF INL-IDDISTR = 9111                                            
015630            MOVE +1 TO WS-IDDC-IX                                         
015640            PERFORM UNTIL (INL-IDLEVNR = TAB-IDLEVNR (WS-IDDC-IX))        
015650                           OR WS-IDDC-IX > MAX-IDDC-IX                    
015660              ADD +1 TO WS-IDDC-IX                                        
015670            END-PERFORM                                                   
015671            IF TAB-IDDC(WS-IDDC-IX) NOT = SPACES                          
015672             MOVE INL-DAINLEV     TO UT-DAINLEV                           
015673             MOVE INL-PRARTNTO    TO UT-PRARTNTO                          
015674             MOVE INL-KVAVIS      TO UT-KVAVIS                            
015675             MOVE INL-KDVALISO    TO UT-KDVALISO                          
015676             MOVE INL-IDDISTR     TO UT-IDDISTR                           
015677             MOVE INL-IDFAKT      TO UT-IDFAKT                            
015680             MOVE TAB-IDDC(WS-IDDC-IX) TO UT-IDDC                         
015700             PERFORM S11-SKRIV-UTFIL                                      
015710            END-IF                                                        
015800         END-IF                                                           
015900         IF INL-IDDISTR = 9161                                            
016000            MOVE +1 TO WS-IDDC-IX                                         
016100            PERFORM UNTIL (INL-IDLEVNR = TAB-IDLEVNR (WS-IDDC-IX))        
016200                           OR WS-IDDC-IX > MAX-IDDC-IX                    
016300              ADD +1 TO WS-IDDC-IX                                        
016400            END-PERFORM                                                   
016500            IF TAB-IDDC(WS-IDDC-IX) NOT = SPACES                          
016600             MOVE INL-DAINLEV     TO UT-DAINLEV                           
016700             MOVE INL-PRARTNTO    TO UT-PRARTNTO                          
016800             MOVE INL-KVAVIS      TO UT-KVAVIS                            
016900             MOVE INL-KDVALISO    TO UT-KDVALISO                          
017000             MOVE INL-IDDISTR     TO UT-IDDISTR                           
017100             MOVE INL-IDFAKT      TO UT-IDFAKT                            
017200             MOVE TAB-IDDC(WS-IDDC-IX) TO UT-IDDC                         
017300             PERFORM S11-SKRIV-UTFIL                                      
017310            END-IF                                                        
017311         END-IF                                                           
017312         IF INL-IDDISTR = 9162                                            
017313            MOVE +1 TO WS-IDDC-IX                                         
017314            PERFORM UNTIL (INL-IDLEVNR = TAB-IDLEVNR (WS-IDDC-IX))        
017315                           OR WS-IDDC-IX > MAX-IDDC-IX                    
017316              ADD +1 TO WS-IDDC-IX                                        
017317            END-PERFORM                                                   
017318            IF TAB-IDDC(WS-IDDC-IX) NOT = SPACES                          
017319             MOVE INL-DAINLEV     TO UT-DAINLEV                           
017320             MOVE INL-PRARTNTO    TO UT-PRARTNTO                          
017321             MOVE INL-KVAVIS      TO UT-KVAVIS                            
017322             MOVE INL-KDVALISO    TO UT-KDVALISO                          
017323             MOVE INL-IDDISTR     TO UT-IDDISTR                           
017324             MOVE INL-IDFAKT      TO UT-IDFAKT                            
017325             MOVE TAB-IDDC(WS-IDDC-IX) TO UT-IDDC                         
017326             PERFORM S11-SKRIV-UTFIL                                      
017327            END-IF                                                        
017328         END-IF                                                           
017330         IF INL-IDDISTR = 9211                                            
017382            MOVE +1 TO WS-IDDC-IX                                         
017383            PERFORM UNTIL (INL-IDLEVNR = TAB-IDLEVNR (WS-IDDC-IX))        
017384                           OR WS-IDDC-IX > MAX-IDDC-IX                    
017385              ADD +1 TO WS-IDDC-IX                                        
017386            END-PERFORM                                                   
017387            IF TAB-IDDC(WS-IDDC-IX) NOT = SPACES                          
017388             MOVE TAB-IDDC(WS-IDDC-IX) TO UT-IDDC                         
017389             MOVE INL-DAINLEV     TO UT-DAINLEV                           
017390             MOVE INL-PRARTNTO    TO UT-PRARTNTO                          
017391             MOVE INL-KVAVIS      TO UT-KVAVIS                            
017392             MOVE INL-KDVALISO    TO UT-KDVALISO                          
017393             MOVE INL-IDDISTR     TO UT-IDDISTR                           
017394             MOVE INL-IDFAKT      TO UT-IDFAKT                            
017395             PERFORM S11-SKRIV-UTFIL                                      
017396            END-IF                                                        
017397         END-IF                                                           
017398         IF INL-IDDISTR = 9261                                            
017399            MOVE +1 TO WS-IDDC-IX                                         
017400            PERFORM UNTIL (INL-IDLEVNR = TAB-IDLEVNR (WS-IDDC-IX))        
017401                           OR WS-IDDC-IX > MAX-IDDC-IX                    
017402              ADD +1 TO WS-IDDC-IX                                        
017403            END-PERFORM                                                   
017404            IF TAB-IDDC(WS-IDDC-IX) NOT = SPACES                          
017405             MOVE TAB-IDDC(WS-IDDC-IX) TO UT-IDDC                         
017406             MOVE INL-DAINLEV     TO UT-DAINLEV                           
017407             MOVE INL-PRARTNTO    TO UT-PRARTNTO                          
017408             MOVE INL-KVAVIS      TO UT-KVAVIS                            
017409             MOVE INL-KDVALISO    TO UT-KDVALISO                          
017410             MOVE INL-IDDISTR     TO UT-IDDISTR                           
017411             MOVE INL-IDFAKT      TO UT-IDFAKT                            
017412             PERFORM S11-SKRIV-UTFIL                                      
017413            END-IF                                                        
017414         END-IF                                                           
017415         IF INL-IDDISTR = 9262                                            
017416            MOVE +1 TO WS-IDDC-IX                                         
017417            PERFORM UNTIL (INL-IDLEVNR = TAB-IDLEVNR (WS-IDDC-IX))        
017418                           OR WS-IDDC-IX > MAX-IDDC-IX                    
017419              ADD +1 TO WS-IDDC-IX                                        
017420            END-PERFORM                                                   
017421            IF TAB-IDDC(WS-IDDC-IX) NOT = SPACES                          
017422             MOVE TAB-IDDC(WS-IDDC-IX) TO UT-IDDC                         
017423             MOVE INL-DAINLEV     TO UT-DAINLEV                           
017424             MOVE INL-PRARTNTO    TO UT-PRARTNTO                          
017425             MOVE INL-KVAVIS      TO UT-KVAVIS                            
017426             MOVE INL-KDVALISO    TO UT-KDVALISO                          
017427             MOVE INL-IDDISTR     TO UT-IDDISTR                           
017428             MOVE INL-IDFAKT      TO UT-IDFAKT                            
017429             PERFORM S11-SKRIV-UTFIL                                      
017430            END-IF                                                        
017431         END-IF                                                           
017432         IF INL-IDDISTR = 9361                                            
017433            MOVE +1 TO WS-IDDC-IX                                         
017434            PERFORM UNTIL (INL-IDLEVNR = TAB-IDLEVNR (WS-IDDC-IX))        
017435                           OR WS-IDDC-IX > MAX-IDDC-IX                    
017436              ADD +1 TO WS-IDDC-IX                                        
017437            END-PERFORM                                                   
017438            IF TAB-IDDC(WS-IDDC-IX) NOT = SPACES                          
017439             MOVE TAB-IDDC(WS-IDDC-IX) TO UT-IDDC                         
017440             MOVE INL-DAINLEV     TO UT-DAINLEV                           
017441             MOVE INL-PRARTNTO    TO UT-PRARTNTO                          
017442             MOVE INL-KVAVIS      TO UT-KVAVIS                            
017443             MOVE INL-KDVALISO    TO UT-KDVALISO                          
017444             MOVE INL-IDDISTR     TO UT-IDDISTR                           
017445             MOVE INL-IDFAKT      TO UT-IDFAKT                            
017446             PERFORM S11-SKRIV-UTFIL                                      
017447            END-IF                                                        
017448         END-IF                                                           
017449         IF INL-IDDISTR = 9362                                            
017450            MOVE +1 TO WS-IDDC-IX                                         
017451            PERFORM UNTIL (INL-IDLEVNR = TAB-IDLEVNR (WS-IDDC-IX))        
017452                           OR WS-IDDC-IX > MAX-IDDC-IX                    
017453              ADD +1 TO WS-IDDC-IX                                        
017454            END-PERFORM                                                   
017455            IF TAB-IDDC(WS-IDDC-IX) NOT = SPACES                          
017456             MOVE TAB-IDDC(WS-IDDC-IX) TO UT-IDDC                         
017457             MOVE INL-DAINLEV     TO UT-DAINLEV                           
017458             MOVE INL-PRARTNTO    TO UT-PRARTNTO                          
017459             MOVE INL-KVAVIS      TO UT-KVAVIS                            
017460             MOVE INL-KDVALISO    TO UT-KDVALISO                          
017461             MOVE INL-IDDISTR     TO UT-IDDISTR                           
017462             MOVE INL-IDFAKT      TO UT-IDFAKT                            
017463             PERFORM S11-SKRIV-UTFIL                                      
017464            END-IF                                                        
017465         END-IF                                                           
017470       END-IF                                                             
017500     END-IF                                                               
021100     .                                                                    
021200     EJECT                                                                
021300                                                                          
021400 Z-FINIT SECTION.                                                         
021500     CLOSE UTFIL                                                          
021800     MOVE 'S' TO POSTSUM-OPKOD                                            
021900     CALL POSTSUM USING POSTSUM-PARM                                      
022000     .                                                                    
022100     SKIP3                                                                
022200                                                                          
022300 S11-SKRIV-UTFIL SECTION.                                                 
022400     WRITE UT-POST FROM UT-AREA                                           
022500     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
022600     MOVE 'UTFIL'    TO POSTSUM-FDNAMN                                    
022700     MOVE 'W51250D1' TO POSTSUM-DDNAMN2                                   
022800     CALL POSTSUM USING POSTSUM-PARM                                      
022900     .                                                                    
023000     EJECT                                                                
023100                                                                          
024900                                                                          
025000* --- IMS SEKTIONER ---                                                   
025100 IMS-GET-WDL6 SECTION.                                                    
025200     CALL CBLTDLI USING GN WDL6-PCB DLI-IO-WDL6                           
025300     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
025400     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
025500     PERFORM IMS-STATUSKONTROLL                                           
025600     .                                                                    
025700     SKIP3                                                                
025710 IMS-GN-WDB601    SECTION.                                                
025720     MOVE 'WDB601  ' TO SSA1                                              
025730     MOVE '  GB'     TO GODK-STATUSKODER                                  
025740     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
025750     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
025760     PERFORM IMS-STATUSKONTROLL                                           
025770     .                                                                    
025780     SKIP3                                                                
025800                                                                          
025900 IMS-STATUSKONTROLL SECTION.                                              
026000     SET STATUS-IX TO 1                                                   
026100     SEARCH GODK-STATUS                                                   
026200       AT END                                                             
026300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
026400           DELIMITED BY SIZE INTO FELTEXT                                 
026500         DISPLAY FELTEXT                                                  
026600         CALL FELLOG                                                      
026700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026800         CONTINUE                                                         
026900     END-SEARCH                                                           
027000     .                                                                    
