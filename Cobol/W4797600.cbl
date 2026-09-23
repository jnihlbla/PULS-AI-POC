000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4797600.                                        
000400 AUTHOR.                 SVANTE BJÖRKBERG.                                
000500 DATE-WRITTEN.           AUG 1987.                                        
000600                                                                          
000700*REMARKS.                                                                 
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET UPPDATERAR WLORDM-BASEN MED FÄRDIGPACK.               
001100*        ORDER FAKT. EL. LAST. UNDER VECKAN.                              
001200*                ORDERBEKRÄFTELSER RENSADE OCH SKRIVNA PÅ                 
001300*                FICHE                                                    
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 - OM RETURKOD FRÅN SORT (EX.VIS FÖR LITE SORTWK)           
001700*                                                                         
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000 CONFIGURATION SECTION.                                                   
002100 SPECIAL-NAMES.                                                           
002200     ALPHABET Y2000 IS X'05' THRU X'09' X'00' THRU X'04'.                 
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*    ---- INFILER:                                                        
002800*                            - UPPDATERINGSTRANS                          
002900     SELECT  W47975        ASSIGN  W47976D1.                              
003000                                                                          
003100*    ---- SORTFIL:                                                        
003200*                                                                         
003300     SELECT  SORTFIL       ASSIGN  W47976DS.                              
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800                                                                          
003900 FD  W47975                                                               
004000     LABEL RECORD STANDARD                                                
004100     RECORDING  F                                                         
004200     BLOCK CONTAINS 0.                                                    
004300                                                                          
004400*    -COPY W4797401      -L.                                              
004500     EJECT                                                                
004600 SD  SORTFIL                                                              
004700     RECORDING V.                                                         
004800                                                                          
004900*01  POST   -COPY W4797401     -PRE SORT-.                                
005000*Y2K-SORT                                                                 
005100     05  FILLER REDEFINES SORT-TIFAKT.                                    
005200       07  SORT-DECADE       PIC X.                                       
005300       07  SORT-SMALL-DATE   PIC S9(5) COMP-3.                            
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800*    ---- GENERERAT PROGRAM-NAMN                                          
005900 77  PROGRAM-NAMN            PIC X(08) VALUE 'W4797600'.                  
006000                                                                          
006100*    ---- KONSTANTER                                                      
006200                                                                          
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500 77  W-CHKP-RAKNARE              PIC S9(5)   VALUE +0    COMP-3.          
006600 77  W-CHKP-MAX                  PIC S9(5)   VALUE +800  COMP-3.          
006700 77  CHKP-ID                     PIC X(8)    VALUE 'W4797600'.            
006800 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
006900 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
007000 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
007100 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
610001                                                                          
610002*    ---- END-OF-FILE SWITCHAR                                            
610003                                                                          
610004 77  SORTFIL-EOF             PIC X       VALUE 'N'.                       
610005                                                                          
610006*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
610007                                                                          
610008 01  DYNAMISKA-SUBPROGRAM.                                                
610009   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
610010   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
610011   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
610012   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
610013                                                                          
610014*    ---- PARAMETRAR TILL ABEND                                           
610015                                                                          
610016 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
610017     EJECT                                                                
610018*    ----  PARAMETRAR TILL POSTSUM                                        
610019                                                                          
610020*01  -COPY W0005       -PRE POSTSUM-.                                     
610021     EJECT                                                                
610022*    ----  AREA FÖR SORTERADE TRANSAKTIONER                               
610023                                                                          
610024 01  FILLER                  PIC X(24) VALUE 'WSORT-AREA-START'.          
610025                                                                          
610026*01  AREA  -COPY W4797401    -PRE  WSORT-.                                
610027     EJECT                                                                
610028*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
610029                                                                          
610030 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
610031                                                                          
610032*    ---- STATUSKOD FRÅN IMS                                              
610033                                                                          
610034 01  STATUS-WS               PIC XX.                                      
610035     88  SEGMENT-FINNS                    VALUE '  '.                     
610036     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
610037     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
610038                                                                          
610039 01  GODK-STATUSKODER.                                                    
610040   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
610041                                                                          
610042 01  SSA1                    PIC X(64).                                   
610043 01  SSA2                    PIC X(64).                                   
610044 01  SSA3                    PIC X(64).                                   
610045                                                                          
610046*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
610047                                                                          
610048 01  NYCKLAR-TILL-DLI.                                                    
610049                                                                          
610050   03  W-WDL101KY-X.                                                      
610051     05  W-IDDISTR           PIC S9(5)    COMP-3.                         
610052     05  W-IDKUNDNR          PIC S9(7)    COMP-3.                         
610053     05  W-IDKUNDRF          PIC X(10).                                   
610054     05  W-IDDC              PIC X(2).                                    
610055     EJECT                                                                
610056*01  -COPY W0003                                                          
610057     EJECT                                                                
610058 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
610059                                                                          
610060 01  DLI-IO-AREA.                                                         
610061   03  IO-AREA               PIC X(100).                                  
610062                                                                          
610063*03  RLMALA01 -COPY WDL101         -RED IO-AREA                           
610064     EJECT                                                                
610065*03  RLMALA02 -COPY WDL111         -RED IO-AREA                           
610066     EJECT                                                                
610067*03  RLMALA03 -COPY WDL112         -RED IO-AREA                           
610068     EJECT                                                                
610069 LINKAGE SECTION.                                                         
610070*01  -COPY W0009      -PRE  MSG-                                          
610071                                                                          
610072*01  -COPY W0008      -PRE  WDL1-                                         
610073       05  FILLER                PIC X.                                   
610074     EJECT                                                                
610075 PROCEDURE DIVISION  USING  MSG-PCB WDL1-PCB.                             
610076     ENTRY 'DLITCBL' USING  MSG-PCB WDL1-PCB.                             
610077                                                                          
610078     PERFORM A-INIT                                                       
610079                                                                          
610080     SORT SORTFIL                                                         
610081       ASCENDING SORT-IDDISTR                                             
610082       ASCENDING SORT-IDKUNDNR                                            
610083       ASCENDING SORT-IDKUNDRF                                            
610084       ASCENDING SORT-IDDC                                                
610085       ASCENDING SORT-IDSEGM                                              
610086       ASCENDING SORT-DECADE                                              
610087       ASCENDING SORT-SMALL-DATE                                          
610088       COLLATING SEQUENCE Y2000                                           
610089       USING W47975                                                       
610090       OUTPUT PROCEDURE B-UPPDATERA-BASEN                                 
610091                                                                          
610092     IF  SORT-RETURN > ZERO                                               
610093       DISPLAY '*** W4797600 - FEL VID SORTERING'                         
610094       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
610095     ELSE                                                                 
610096       PERFORM Z-FINIT                                                    
610097       MOVE ZERO TO RETURN-CODE                                           
610098       GOBACK                                                             
610099     END-IF.                                                              
610100     EJECT                                                                
610101 A-INIT SECTION.                                                          
610102                                                                          
610103     PERFORM IMS-RESTART                                                  
610104                                                                          
610105     MOVE ZERO         TO W-CHKP-RAKNARE                                  
610106     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN.                               
610107     EJECT                                                                
610108 B-UPPDATERA-BASEN SECTION.                                               
610109                                                                          
610110     PERFORM BA-LAS-SORTERAD-W47975                                       
610111                                                                          
610112     PERFORM UNTIL SORTFIL-EOF = JA                                       
610113       PERFORM BB-UPPD-WDL111-ELLER-WDL112                                
610114                                                                          
610115       IF SEGMENT-SAKNAS                                                  
610116         PERFORM BC-UPPD-WDL101                                           
610117         PERFORM BB-UPPD-WDL111-ELLER-WDL112                              
610118       END-IF                                                             
610119                                                                          
610120       IF W-CHKP-RAKNARE       >  W-CHKP-MAX                              
610121          PERFORM IMS-CHECKPOINT                                          
610122          MOVE ZERO            TO W-CHKP-RAKNARE                          
610123       END-IF                                                             
610124                                                                          
610125       PERFORM BA-LAS-SORTERAD-W47975                                     
610126     END-PERFORM.                                                         
610127     EJECT                                                                
610128 BB-UPPD-WDL111-ELLER-WDL112 SECTION.                                     
610129                                                                          
610130     MOVE WSORT-IDDISTR            TO W-IDDISTR                           
610131     MOVE WSORT-IDKUNDNR           TO W-IDKUNDNR                          
610132     MOVE WSORT-IDKUNDRF           TO W-IDKUNDRF                          
610133     MOVE WSORT-IDDC               TO W-IDDC                              
610134     IF WSORT-IDSEGM = 'WDL111'                                           
610135        MOVE WSORT-TIFAKT          TO DAT-DAFAKT                          
610136        IF WSORT-TIFAKT < 500000                                          
610137           MOVE 20 TO DAT-DAFAKT (1:2)                                    
610138        ELSE                                                              
610139           IF WSORT-TIFAKT < 999999                                       
610140              MOVE 19 TO DAT-DAFAKT (1:2)                                 
610141           ELSE                                                           
610142              MOVE 99999999 TO DAT-DAFAKT                                 
610143           END-IF                                                         
610144        END-IF                                                            
610145        PERFORM IMS-INSERT-WDL111                                         
610146     ELSE                                                                 
610147        MOVE WSORT-TIHIST-OBKR     TO DAT-DAHISTOB                        
610148        IF WSORT-TIHIST-OBKR < 500000                                     
610149           MOVE 20 TO DAT-DAHISTOB (1:2)                                  
610150        ELSE                                                              
610151           IF WSORT-TIHIST-OBKR < 999999                                  
610152              MOVE 19 TO DAT-DAHISTOB (1:2)                               
610153           ELSE                                                           
610154              MOVE 99999999 TO DAT-DAHISTOB                               
610155           END-IF                                                         
610156        END-IF                                                            
610157        PERFORM IMS-INSERT-WDL112                                         
610158     END-IF                                                               
610159                                                                          
610160     ADD +1       TO W-CHKP-RAKNARE                                       
610161     .                                                                    
610162     EJECT                                                                
610163                                                                          
610164 BC-UPPD-WDL101 SECTION.                                                  
610165                                                                          
610166     MOVE WSORT-IDDISTR            TO ORD-IDDISTR                         
610167     MOVE WSORT-IDKUNDNR           TO ORD-IDKUNDNR                        
610168     MOVE WSORT-IDKUNDRF           TO ORD-IDKUNDRF                        
610169     MOVE WSORT-IDDC               TO ORD-IDDC                            
610170     PERFORM IMS-INSERT-WDL101                                            
610171                                                                          
610172     ADD +1       TO W-CHKP-RAKNARE                                       
610173     .                                                                    
610174     EJECT                                                                
610175                                                                          
610176 BA-LAS-SORTERAD-W47975 SECTION.                                          
610177                                                                          
610178     RETURN SORTFIL INTO WSORT-AREA                                       
610179     AT END                                                               
610180        MOVE JA TO SORTFIL-EOF                                            
610181     END-RETURN                                                           
610182                                                                          
610183     IF SORTFIL-EOF = NEJ                                                 
610184       MOVE 'W47975'         TO POSTSUM-FDNAMN                            
610185       MOVE 'W47976D1'       TO POSTSUM-DDNAMN2                           
610186       MOVE SPACE            TO POSTSUM-TRANSTYP                          
610187       CALL POSTSUM USING POSTSUM-PARM                                    
610188     END-IF.                                                              
610189     EJECT                                                                
610190 Z-FINIT   SECTION.                                                       
610191                                                                          
610192*    ----  SKRIV UT ANTAL LÄSTA OCH SKRIVNA POSTER                        
610193     MOVE 'S' TO POSTSUM-OPKOD                                            
610194     CALL POSTSUM USING POSTSUM-PARM.                                     
610195     EJECT                                                                
610196*    ---- IMS SEKTIONER                                                   
610197                                                                          
610198 IMS-RESTART  SECTION.                                                    
610199                                                                          
610200     MOVE SPACE TO MSG-IO-AREA-1                                          
610201     MOVE '  ' TO GODK-STATUSKODER                                        
610202     CALL CBLTDLI USING XRST MSG-PCB                                      
610203                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
610204                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
610205     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
610206     PERFORM IMS-STATUSKONTROLL                                           
610207     .                                                                    
610208                                                                          
610209     SKIP3                                                                
610210 IMS-CHECKPOINT  SECTION.                                                 
610211                                                                          
610212     MOVE CHKP-ID TO MSG-IO-AREA-1                                        
610213     MOVE '  XD' TO GODK-STATUSKODER                                      
610214     CALL CBLTDLI USING CHKP MSG-PCB                                      
610215                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
610216                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
610217     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
610218     PERFORM IMS-STATUSKONTROLL                                           
610219     .                                                                    
610220     EJECT                                                                
610221 IMS-INSERT-WDL101   SECTION.                                             
610222                                                                          
610223     MOVE 'WDL101   '           TO SSA1                                   
610224     MOVE '  GE'                TO GODK-STATUSKODER                       
610225     CALL CBLTDLI USING ISRT  WDL1-PCB DLI-IO-AREA SSA1                   
610226     MOVE WDL1-STATUS-CODE      TO STATUS-WS                              
610227     PERFORM IMS-STATUSKONTROLL.                                          
610228     SKIP3                                                                
610229 IMS-INSERT-WDL111   SECTION.                                             
610230                                                                          
610231     STRING 'WDL101  (WDL101KY =' W-WDL101KY-X ')'                        
610232            DELIMITED BY SIZE INTO SSA1                                   
610233     MOVE 'WDL111   ' TO SSA2                                             
610234     MOVE '  IIGE' TO GODK-STATUSKODER                                    
610235     CALL CBLTDLI USING ISRT  WDL1-PCB DLI-IO-AREA SSA1 SSA2              
610236     MOVE WDL1-STATUS-CODE TO STATUS-WS                                   
610237     PERFORM IMS-STATUSKONTROLL.                                          
610238                                                                          
610239 IMS-INSERT-WDL112   SECTION.                                             
610240                                                                          
610241     STRING 'WDL101  (WDL101KY =' W-WDL101KY-X ')'                        
610242            DELIMITED BY SIZE INTO SSA1                                   
610243     MOVE 'WDL112   ' TO SSA2                                             
610244     MOVE '  IIGE' TO GODK-STATUSKODER                                    
610245     CALL CBLTDLI USING ISRT  WDL1-PCB DLI-IO-AREA SSA1 SSA2              
610246     MOVE WDL1-STATUS-CODE TO STATUS-WS                                   
610247     PERFORM IMS-STATUSKONTROLL.                                          
610248     EJECT                                                                
610249 IMS-STATUSKONTROLL SECTION.                                              
610250                                                                          
610251     SET STATUS-IX TO 1                                                   
610252     SEARCH GODK-STATUS                                                   
610253       AT END CALL FELLOG                                                 
610254       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
610255     END-SEARCH.                                                          
