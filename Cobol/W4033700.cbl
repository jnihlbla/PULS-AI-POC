000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4033700.                                                
000400 AUTHOR.         CAP GEMINI / BOH                                         
000500     DATE-WRITTEN.   FEB   86.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*    PROGRAMMET ÄR ETT RENT UPPDATERINGSPROGRAM.                          
001100*    DET UPPDATERAR BRUTTOVIKTEN VID KOLLIRAPPORTERING                    
001200*    VID BANDSTATION.                                                     
001300*    EFTER DET ATT UPPDATERINGEN ÄR GJORD, LÄGGS                          
001400*    W4O33101 -MODEN UT SÅ ATT NYTT KOLLI KAN RAPPORTERAS.                
001410*    DESSUTOM STARTAS 4333 FÖR ATT SKRIVA ADRESSFLAGGA.                   
001500*                                                                         
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W4T337                                              
001900*        MID:         W4I33701                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W4O33701                                            
002300*                     W4O33101                                            
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
002901                                                                          
002910*    -- CHECKED BY WY2000                                                 
003000 77   PROGRAM-NAMN           VALUE 'W4033700'                             
003100                                 PIC X(8).                                
003200 77  JA                          PIC X(1)    VALUE 'J'.                   
003300 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003400 77  INDX                        PIC S9(4)   COMP SYNC.                   
003500 77  WS-IDDISTR                  PIC X(4).                                
003600 77  WS-IDKUNDNR                 PIC X(6).                                
003700 77  WS-IDKOLLI                  PIC X(5).                                
003720 77  WS-KDPRTVAL                 PIC XX.                                  
003800 77  WS-VKORDBTO                 PIC S9(6)V9 COMP-3.                      
003900 77  WS-IDPRODNR                 PIC  9(7)   VALUE ZERO.                  
003910 77  WS-IDPRODNR-RED             PIC Z(6)9   VALUE ZERO.                  
004000 77  MOD-4337-LAENGD             PIC S9(4)   VALUE +179 COMP SYNC.        
004100 77  MOD-4331-LAENGD             PIC S9(4)   VALUE +206 COMP SYNC.        
004200 77  MOD-LAENGD-OEVRIGA          PIC S9(4)   VALUE +48  COMP SYNC.        
004300 77  4342-MID-TRANS-LAENGD       PIC S9(4)   VALUE +37  COMP SYNC.        
004400     SKIP3                                                                
004500 01  DYNAMISKA-SUBPROGRAM.                                                
004600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
004801     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
004802                                                                          
004803*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
004804*01 -COPY WMSGINIT                                                        
004805     EJECT                                                                
004806*      --- VALID IDDC CODES                                               
004807*                                                                         
004808*01    -COPY WWDC99                                                       
004809       EJECT                                                              
005100 01  WS-IDKUNDRF.                                                         
005200                                                                          
005300     03  WS-IDORDNR              PIC X(5).                                
005400     03  FILLER                  PIC X(5)  VALUE SPACE.                   
005500     SKIP3                                                                
005600 01  WS-FEL-FUNNET               PIC X     VALUE 'N'.                     
005700                                                                          
005800     88  FEL-FUNNET                        VALUE 'J'.                     
005900     88  FEL-EJ-FUNNET                     VALUE 'N'.                     
006000     SKIP3                                                                
006100 01  WS-IDTRANS                  PIC X(4)  VALUE '4337'.                  
006200                                                                          
006300     88  GODKAEND-BILD                     VALUE '4337'                   
006400                                                 '433A'.                  
006500     EJECT                                                                
007000 01  NYCKLAR-TILL-DLI.                                                    
007100                                                                          
007200     03  W-E601-IDPRODNR-X.                                               
007300                                                                          
007400         05  W-E601-IDPRODNR     PIC S9(7)   COMP-3.                      
007500                                                                          
007600     03  W-E611-IDKOLLI-X.                                                
007700                                                                          
007800         05  W-E611-IDKOLLI      PIC S9(5)   COMP-3.                      
007810                                                                          
007820     03  W-IDDC-B6-X.                                                     
007830         05 W-IDDC-B6                  PIC X(2).                          
007900     EJECT                                                                
008000 01  MEDDELANDE.                                                          
008100                                                                          
008200     03  MED-1.                                                           
008300         05  FILLER              PIC X(40)   VALUE                        
008400             'UPPDATERING UTFÖRD                      '.                  
008500         05  FILLER              PIC X(40)   VALUE                        
008610             'UPDATING PERFORMED                      '.                  
008700     03  MED1 REDEFINES MED-1 OCCURS 2 PIC X(40).                         
008800                                                                          
008900     03  FEL-1.                                                           
009000         05  FILLER              PIC X(40)   VALUE                        
009100             '748 UPPLYSTA FÄLT FEL                   '.                  
009200         05  FILLER              PIC X(40)   VALUE                        
009310             '748 HIGHLITH FIELDS WRONG               '.                  
009400     03  FEL-748 REDEFINES FEL-1 OCCURS 2 PIC X(40).                      
009500                                                                          
009600     03  FEL-2.                                                           
009700         05  FILLER              PIC X(40)   VALUE                        
009800             '724 NOLL FÅR EJ ANGES                   '.                  
009900         05  FILLER              PIC X(40)   VALUE                        
010010             '724 ZERO NOT ALLOWED                    '.                  
010100     03  FEL-724 REDEFINES FEL-2 OCCURS 2 PIC X(40).                      
010200                                                                          
010300     03  FEL-3.                                                           
010400         05  FILLER              PIC X(40)   VALUE                        
010500             '786 EJ TILLÅTEN ATT STARTA 4337         '.                  
010600         05  FILLER              PIC X(40)   VALUE                        
010700             '786 START SCREEN 43372 FROM MENU.       '.                  
010800     03  FEL-786 REDEFINES FEL-3 OCCURS 2 PIC X(40).                      
010801                                                                          
010802     03  FEL-4.                                                           
010803         05  FILLER              PIC X(40)   VALUE                        
010804             '749 FEL NYCKEL                          '.                  
010805         05  FILLER              PIC X(40)   VALUE                        
010806             '749 WRONG KEY                           '.                  
010807     03  FEL-749 REDEFINES FEL-4 OCCURS 2 PIC X(40).                      
010810                                                                          
010900     EJECT                                                                
011000*01  -COPY WDECAREA                                                       
011200     EJECT                                                                
011300 01    FILLER              PIC X(16)   VALUE 'MFS-WS'.                    
011400                                                                          
011500                                                                          
011600                                                                          
011700 01    FILLER              PIC X(16)   VALUE 'MID W4I337 MID'.            
011800*01  MID -COPY W4I33701.                                                  
012000     EJECT                                                                
012100*01  -COPY WMSGAREA                                                       
012300     EJECT                                                                
012400*    03  MOD -COPY W4O33701  -RED MSG-AREA.                               
012600     EJECT                                                                
012700*01  MOD -COPY W4O33101  -PRE 4331-.                                      
012900     EJECT                                                                
012901 01  4333-MID-IO-AREA.                                                    
012902                                                                          
012903     03  4333-MID-LL             PIC S9(4)   COMP SYNC.                   
012904     03  4333-MID-Z1             PIC X.                                   
012905     03  4333-MID-Z2             PIC X.                                   
012906     03  4333-MID-TRANSKOD       PIC X(8)    VALUE 'W4T333  '.            
012907     03  4333-MID-IDTRANS        PIC X(4)    VALUE '433A'.                
012908     03  4333-MID-KDMFSFOR       PIC X.                                   
012910*    03  MID -COPY W4I33301    -PRE 4333-.                                
012920     EJECT                                                                
013000 01  4342-MID-IO-AREA.                                                    
013100                                                                          
013200     03  4342-MID-LL            PIC S9(4)  COMP SYNC.                     
013300     03  4342-MID-Z1            PIC X.                                    
013400     03  4342-MID-Z2            PIC X.                                    
013500     03  4342-MID-TRANSKOD      PIC X(8)   VALUE 'W4T342U'.               
013600     03  4342-MID-IDTRANS       PIC X(4)   VALUE '433G'.                  
013700     03  4342-MID-KDMFSFOR      PIC X.                                    
013800     03  4342-MID-DATA-AREA     PIC X(18).                                
013900*    03  MID  -COPY W4I34201 -RED 4342-MID-DATA-AREA  -PRE 4342-.         
014100     EJECT                                                                
014200*01  -COPY WMFSAREA                                                       
014400     EJECT                                                                
014500 01  IMS-WS.                                                              
014600     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
014700     SKIP3                                                                
014800*                        **** STATUS-KOD FRÅN IMS                         
014900     03  STATUS-WS               PIC X(2).                                
015000         88  SEGMENT-FINNS                   VALUE '  '.                  
015100         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
015200     SKIP3                                                                
015300     03  GODK-STATUSKODER.                                                
015400         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
015500     SKIP3                                                                
015600 01  SSA1                        PIC X(64).                               
015700 01  SSA2                        PIC X(64).                               
015800     EJECT                                                                
015900*                            IMS FUNKTIONSKODER                           
016000*01  -COPY W0003                                                          
016200     EJECT                                                                
016300*                            DLI INPUT-OUTPUT AREA                        
016400 01  DLI-IO-AREA.                                                         
016500     03  IO-AREA                 PIC X(608)  VALUE SPACE.                 
016600                                                                          
016700                                                                          
016800*    03  WDE601 -COPY WDE601     -RED IO-AREA.                            
017000     EJECT                                                                
017100*    03  WDE611 -COPY WDE611     -RED IO-AREA.                            
017200                                                                          
017210 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
017220 01   DLI-IO-AREA-B601.                                                   
017230*     03  -COPY WDB601                                                    
017300     EJECT                                                                
017400 LINKAGE SECTION.                                                         
017500*01  -COPY W0009     -PRE MSG-                                            
017700     EJECT                                                                
017800*01  -COPY W0009     -PRE ALT-                                            
018000     EJECT                                                                
018010*01  -COPY W0009     -PRE 4333-                                           
018020     EJECT                                                                
018030*01  -COPY W0009     -PRE 4341-                                           
018040     EJECT                                                                
018100*01  -COPY W0008     -PRE WDE6-                                           
018300     05  FILLER                  PIC X.                                   
018400     EJECT                                                                
018410*01  -COPY W0008     -PRE WDB6-                                           
018420     05  FILLER                  PIC X.                                   
018430     EJECT                                                                
018500 PROCEDURE DIVISION USING MSG-PCB  ALT-PCB  4333-PCB                      
018600                                   WDE6-PCB WDB6-PCB.                     
018700                                                                          
018800     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  4333-PCB                     
018810                                    WDE6-PCB WDB6-PCB.                    
018900                                                                          
019000                                                                          
019100     PERFORM IMS-GET-MSG                                                  
019200                                                                          
019300     IF SEGMENT-FINNS                                                     
019400         MOVE NEJ       TO WS-FEL-FUNNET                                  
019500         PERFORM A-INITIERA                                               
019501           STRING '*' WS-IDDISTR                                          
019502                  '*' WS-IDKUNDNR                                         
019503                  '*' WS-IDORDNR                                          
019504                  '*' WS-IDPRODNR                                         
019505                  '*********'                                             
019507           DELIMITED BY SIZE INTO MOD-TEMFSFEL                            
019508*BA  CALL FELLOG                                                          
019509         IF WS-IDDISTR  NUMERIC AND WS-IDDISTR  > ZERO AND                
019510            WS-IDKUNDNR NUMERIC AND                                       
019511            WS-IDORDNR  NUMERIC AND WS-IDORDNR  > ZERO AND                
019512            WS-IDKOLLI  NUMERIC AND WS-IDKOLLI  > ZERO                    
019520                                                                          
019700           IF GODKAEND-BILD                                               
019800               PERFORM B-BEHANDLA-BRUTTOVIKT                              
019820           STRING '*' WS-IDDISTR                                          
019830                  '*' WS-IDKUNDNR                                         
019840                  '*' WS-IDORDNR                                          
019850                  '*' WS-IDPRODNR                                         
019870           DELIMITED BY SIZE INTO MOD-TEMFSFEL                            
019880                                                                          
019900                                                                          
020000               IF FEL-EJ-FUNNET                                           
020100                   PERFORM C-UPPDATERA-WDE601                             
020200                   PERFORM D-UPPDATERA-WDE611                             
020210                   PERFORM G-SEND-PRINTTRANS-4333                         
020300                   PERFORM E-REDIGERA-W4O33101-MOD                        
020400               END-IF                                                     
020500           ELSE                                                           
020600               PERFORM F-FELMEDD-TILL-RAETT-MOD                           
020700           END-IF                                                         
020701         ELSE                                                             
020702             PERFORM H-SHOW-4331-MOD                                      
020710         END-IF                                                           
020800                                                                          
020900         PERFORM IMS-INSERT-MSG                                           
021000                                                                          
021100     END-IF                                                               
021200                                                                          
021300     MOVE ZERO TO RETURN-CODE                                             
021400                                                                          
021500     GOBACK                                                               
021600     .                                                                    
021700     EJECT                                                                
021800 A-INITIERA SECTION.                                                      
021900                                                                          
022000                                                                          
022100     IF MSG-DUBBLA-TRANSKODER                                             
022200         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I33701               
022300         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS WS-IDTRANS                     
022400         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
022500         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
022600         MOVE MSG-IDPFK TO MFS-IDPFK                                      
022700     ELSE                                                                 
022800         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I33701                
022900         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS WS-IDTRANS                     
023000         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
023100         MOVE ' ' TO MFS-KDTRTYP        MFS-IDPFK                         
023200     END-IF                                                               
023300                                                                          
023400     MOVE MID-IDDISTR-UT      TO WS-IDDISTR                               
023500     INSPECT WS-IDDISTR REPLACING ALL SPACE BY ZERO                       
023600                                                                          
023700     MOVE MID-IDKUNDNR-UT     TO WS-IDKUNDNR                              
023800     INSPECT WS-IDKUNDNR REPLACING ALL SPACE BY ZERO                      
023900                                                                          
024000     MOVE MID-IDORDNR-UT      TO WS-IDORDNR                               
024100     INSPECT WS-IDORDNR REPLACING ALL SPACE BY ZERO                       
024200                                                                          
024300     MOVE MID-IDKOLLI-UT      TO WS-IDKOLLI                               
024400     INSPECT WS-IDKOLLI REPLACING ALL SPACE BY ZERO                       
024410                                                                          
024411     MOVE MID-IDDC-UT         TO WS-IDDC                                  
024413                                                                          
024420     MOVE MID-KDPRTVAL-AF-UT  TO WS-KDPRTVAL                              
024500                                                                          
024510     MOVE MID-KDPRTVAL-FS     TO MOD-KDPRTVAL-FS                          
024511                                 4331-MOD-KDPRTVAL-FS-UT                  
024520                                                                          
024600     MOVE LOW-VALUE           TO MSG-AREA                                 
024700     MOVE 'W4O337N1'          TO MFS-IDMOD                                
024800     MOVE '4337'              TO MOD-IDTRANS                              
024900     MOVE MOD-4337-LAENGD     TO MSG-KVLL                                 
025000                                                                          
025100     MOVE WS-IDDISTR          TO 4331-MOD-IDDISTR-UT                      
025200     INSPECT 4331-MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE          
025300                                                                          
025400     MOVE WS-IDKUNDNR         TO 4331-MOD-IDKUNDNR-UT                     
025500     INSPECT 4331-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
025600                                                                          
025700     MOVE WS-IDORDNR          TO 4331-MOD-IDORDNR-UT                      
025800     INSPECT 4331-MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE          
025900                                                                          
026000     MOVE WS-IDKOLLI          TO 4331-MOD-IDKOLLI-UT                      
026100     INSPECT 4331-MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE          
026200                                                                          
026210     MOVE WS-IDDC             TO 4331-MOD-IDDC-UT                         
026230                                                                          
026300     IF SWEDISH-TEXT                                                      
026400         MOVE +1 TO INDX                                                  
026500     ELSE                                                                 
026600         MOVE +2 TO INDX                                                  
026700     END-IF                                                               
026800                                                                          
026900     MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-IN                             
027000                               MOD-IDKUNDNR-IN                            
027100                               MOD-IDORDNR-IN                             
027200                               MOD-IDKOLLI-IN                             
027300                               MOD-IDDC-IN                                
027310                               MOD-KDPRTVAL-AF-IN                         
027400                               MOD-TEMFSFEL                               
027500                               MOD-TEMFSINF                               
027600                               4331-MOD-IDDISTR-IN                        
027700                               4331-MOD-IDKUNDNR-IN                       
027800                               4331-MOD-IDORDNR-IN                        
027900                               4331-MOD-IDKOLLI-IN                        
027910                               4331-MOD-IDDC-IN                           
028000                               4331-MOD-KDPRTVAL-AF-IN                    
028010                               4331-MOD-KDPRTVAL-FS-IN                    
028100                               4331-MOD-KDKOLLI                           
028200                               4331-MOD-KDEMBTYP                          
028300                               4331-MOD-DIKOLLIL                          
028400                               4331-MOD-DIKOLLIB                          
028500                               4331-MOD-DIKOLLIH                          
028600                               4331-MOD-ADRESS-TEXT                       
028700                               4331-MOD-ADFLGEO                           
028800                               4331-MOD-ADFLOMR                           
028900                               4331-MOD-ADRUTNIV                          
029000                               4331-MOD-ADVMODUL                          
029100                               4331-MOD-SLUT-TEXT                         
029200                               4331-MOD-TEMFSFEL                          
029300                               4331-MOD-TEMFSINF                          
029400                                                                          
029500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-UT                             
029600                               MOD-IDKUNDNR-UT                            
029700                               MOD-IDORDNR-UT                             
029800                               MOD-IDKOLLI-UT                             
029810                               MOD-IDDC-UT                                
029900                               MOD-KDPRTVAL-AF-UT                         
030000                               MOD-VKORDBTO                               
030100                               MOD-IDPRODNR                               
030200                               MOD-ADRESS-TEXT                            
030300                               MOD-ADFLGEO                                
030400                               MOD-ADFLOMR                                
030500                               MOD-ADRUTNIV                               
030600                               MOD-ADVMODUL                               
030700     .                                                                    
030800     EJECT                                                                
030900 B-BEHANDLA-BRUTTOVIKT SECTION.                                           
031000                                                                          
031100     MOVE MID-VKORDBTO        TO DEC-IDFRIDATA                            
031200     MOVE +6                  TO DEC-KVHELTAL                             
031300     MOVE +1                  TO DEC-KVDECIMAL                            
031400                                                                          
031500     CALL WDECEDIT USING DEC-WDECAREA                                     
031600                                                                          
031700     IF DEC-KDSVAR-FEL                                                    
031800         IF FEL-EJ-FUNNET                                                 
031900             MOVE FEL-748(INDX)     TO MOD-TEMFSFEL                       
032000             MOVE JA                TO WS-FEL-FUNNET                      
032100         END-IF                                                           
032200         MOVE MFS-NUM-FAELT-FEL     TO MOD-VKORDBTO-ATTR                  
032300     ELSE                                                                 
032400         IF DEC-IDEDITDATA = ZERO                                         
032500             IF FEL-EJ-FUNNET                                             
032600                 MOVE FEL-724(INDX) TO MOD-TEMFSFEL                       
032700                 MOVE JA            TO WS-FEL-FUNNET                      
032800             END-IF                                                       
032900             MOVE MFS-NUM-FAELT-FEL TO MOD-VKORDBTO-ATTR                  
033000         END-IF                                                           
033100     END-IF                                                               
033200     .                                                                    
033300     EJECT                                                                
033400 C-UPPDATERA-WDE601 SECTION.                                              
033500                                                                          
033600                                                                          
033700     MOVE MID-IDPRODNR TO W-E601-IDPRODNR                                 
033710                          WS-IDPRODNR                                     
033800     PERFORM IMS-GHU-E601-KVAL                                            
033900                                                                          
034000     ADD DEC-IDEDITDATA         TO VORD-VKORDBTO                          
034100                                                                          
034200     PERFORM IMS-REPLACE-E6                                               
034300                                                                          
034310     MOVE VORD-IDDC     TO WS-IDDC                                        
034320     MOVE VORD-IDDC     TO W-IDDC-B6                                      
034330     PERFORM IMS-GU-WDB601                                                
034401                                                                          
034410     IF VORD-KDORDSTA = 3                                                 
034411        IF DCS-KDPORDL = JA                                               
034416                                                                          
034700           PERFORM CA-SKAPA-PACK-ORDER-LISTA                              
034800        END-IF                                                            
034810     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 CA-SKAPA-PACK-ORDER-LISTA SECTION.                                       
035200                                                                          
035300                                                                          
035400     MOVE '+++++++'             TO 4342-MID-IDPRODNR-IN                   
035410     MOVE '++'                  TO 4342-MID-IDDC-IN                       
035500     MOVE VORD-IDPRODNR         TO WS-IDPRODNR-RED                        
035600     MOVE WS-IDPRODNR-RED       TO 4342-MID-IDPRODNR-UT                   
035610     MOVE VORD-IDDC             TO 4342-MID-IDDC-UT                       
035700                                                                          
035800     MOVE 4342-MID-TRANS-LAENGD TO 4342-MID-LL                            
035900     MOVE 'W4T342U '            TO 4342-MID-TRANSKOD                      
036000     MOVE '433G'                TO 4342-MID-IDTRANS                       
036100     MOVE MFS-KDMFSFOR          TO 4342-MID-KDMFSFOR                      
036200                                                                          
036300     PERFORM IMS-INSERT-ALT-MSG                                           
036400     .                                                                    
036500     EJECT                                                                
036600 D-UPPDATERA-WDE611 SECTION.                                              
036700                                                                          
036800                                                                          
036900     MOVE WS-IDKOLLI            TO W-E611-IDKOLLI                         
037000     PERFORM IMS-GHNP-E611-KVAL                                           
037100                                                                          
037200     MOVE DEC-IDEDITDATA        TO KOLLI-VKORDBTO-KOLLI                   
037300                                                                          
037400     IF KOLLI-VKORDBTO-KOLLI < KOLLI-VKORDNTO-KOLLI                       
037500       MOVE KOLLI-VKORDBTO-KOLLI TO KOLLI-VKORDNTO-KOLLI                  
037600     END-IF                                                               
037700                                                                          
037800     PERFORM IMS-REPLACE-E6                                               
037900     .                                                                    
038000     EJECT                                                                
038100 E-REDIGERA-W4O33101-MOD SECTION.                                         
038200                                                                          
038300                                                                          
038400     MOVE LOW-VALUE             TO MSG-AREA                               
038500     MOVE MOD-4331-LAENGD       TO MSG-KVLL                               
038600     MOVE 'W4O33101'            TO MFS-IDMOD                              
038700     MOVE '4331'                TO 4331-MOD-IDTRANS                       
038800     MOVE MID-KDPRTVAL-AF-UT    TO 4331-MOD-KDPRTVAL-AF-UT                
038810     MOVE MID-KDPRTVAL-AF-UT    TO 4331-MOD-KDPRTVAL-AF-UT                
038900     MOVE MED1(INDX)            TO 4331-MOD-TEMFSINF                      
039000     MOVE MFS-FORMATETS-ATTR    TO 4331-MOD-KDPRTVAL-AF-IN-ATTR           
039020                                   4331-MOD-KDPRTVAL-FS-IN-ATTR           
039100                                   4331-MOD-KDKOLLI-ATTR                  
039200                                   4331-MOD-KDEMBTYP-ATTR                 
039300                                   4331-MOD-DIKOLLIL-ATTR                 
039400                                   4331-MOD-DIKOLLIB-ATTR                 
039500                                   4331-MOD-DIKOLLIH-ATTR                 
039600     MOVE 4331-MOD-W4O33101     TO MSG-AREA                               
039700     .                                                                    
039800     EJECT                                                                
039900 F-FELMEDD-TILL-RAETT-MOD SECTION.                                        
040000                                                                          
040100                                                                          
040200     MOVE MFS-IDTRANS           TO MOD-IDTRANS                            
040300     MOVE MSG-MOD-NAME          TO MFS-IDMOD                              
040400     INSPECT MFS-IDMOD   REPLACING FIRST 'I' BY 'O'                       
040500     MOVE MOD-LAENGD-OEVRIGA    TO MSG-KVLL                               
040600     MOVE FEL-786(INDX)         TO MOD-TEMFSFEL                           
040610     .                                                                    
040700     EJECT                                                                
040710 G-SEND-PRINTTRANS-4333      SECTION.                                     
040720                                                                          
040724     MOVE WS-IDDISTR         TO 4333-MID-IDDISTR-UT                       
040725     MOVE WS-IDKUNDNR        TO 4333-MID-IDKUNDNR-UT                      
040726     MOVE WS-IDORDNR         TO 4333-MID-IDORDNR-UT                       
040727     MOVE WS-IDKOLLI         TO 4333-MID-IDKOLLI-UT                       
040728     MOVE WS-IDDC            TO 4333-MID-IDDC-UT                          
040729*BA                                                                       
040730     MOVE WS-IDPRODNR        TO 4333-MID-IDPRODNR-UT                      
040731     MOVE WS-KDPRTVAL        TO 4333-MID-KDPRTVAL-UT                      
040732     MOVE ZERO               TO 4333-MID-IDKOLLI-TOM                      
040733     MOVE '++++'             TO 4333-MID-IDDISTR-IN                       
040734     MOVE '++++++'           TO 4333-MID-IDKUNDNR-IN                      
040735     MOVE '+++++'            TO 4333-MID-IDORDNR-IN                       
040736                                4333-MID-IDKOLLI-IN                       
040737     MOVE '++'               TO 4333-MID-IDDC-IN                          
040738     MOVE '+++++++'          TO 4333-MID-IDPRODNR-IN                      
040739     MOVE '++'               TO 4333-MID-KDPRTVAL-IN                      
040740                                                                          
040741     COMPUTE 4333-MID-LL = LENGTH OF 4333-MID-W4I33301 + 17               
040743     MOVE 'W4T333  '         TO 4333-MID-TRANSKOD                         
040744     MOVE '433A'             TO 4333-MID-IDTRANS                          
040745     MOVE MFS-KDMFSFOR       TO 4333-MID-KDMFSFOR                         
040746                                                                          
040747     PERFORM IMS-INSERT-ALT-MSG-4333                                      
040748     .                                                                    
040750     EJECT                                                                
040751 H-SHOW-4331-MOD      SECTION.                                            
040752                                                                          
040753     MOVE LOW-VALUE             TO MSG-AREA                               
040754     MOVE MOD-4331-LAENGD       TO MSG-KVLL                               
040755     MOVE 'W4O33101'            TO MFS-IDMOD                              
040756     MOVE '4331'                TO 4331-MOD-IDTRANS                       
040757     MOVE MID-KDPRTVAL-AF-UT    TO 4331-MOD-KDPRTVAL-AF-UT                
040758     MOVE FEL-749(INDX)         TO 4331-MOD-TEMFSINF                      
040759     MOVE MFS-FORMATETS-ATTR    TO 4331-MOD-KDPRTVAL-AF-IN-ATTR           
040760                                   4331-MOD-KDPRTVAL-FS-IN-ATTR           
040761                                   4331-MOD-KDKOLLI-ATTR                  
040762                                   4331-MOD-KDEMBTYP-ATTR                 
040763                                   4331-MOD-DIKOLLIL-ATTR                 
040764                                   4331-MOD-DIKOLLIB-ATTR                 
040765                                   4331-MOD-DIKOLLIH-ATTR                 
040766     .                                                                    
040770     EJECT                                                                
040800* IMS SEKTIONER                                                           
041100 IMS-GET-MSG SECTION.                                                     
041200     MOVE '  QC' TO GODK-STATUSKODER                                      
041300     CALL CBLTDLI USING GU                                                
041400                          MSG-PCB                                         
041500                          MSG-IO-AREA                                     
041600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041700     PERFORM IMS-STATUSKONTROLL                                           
041800     SKIP3                                                                
041900     .                                                                    
042000 IMS-INSERT-MSG SECTION.                                                  
042010                                                                          
042020     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
042030       MOVE '0' TO MFS-KDHUVOMR                                           
042040     END-IF                                                               
042400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
042500     MOVE SPACE TO GODK-STATUSKODER                                       
042600     CALL CBLTDLI USING ISRT                                              
042700                          MSG-PCB                                         
042800                          MSG-IO-AREA                                     
042900                          MFS-IDMOD                                       
043000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043100     PERFORM IMS-STATUSKONTROLL                                           
043200     SKIP2                                                                
043300     .                                                                    
043400 IMS-INSERT-ALT-MSG SECTION.                                              
043500     MOVE LOW-VALUE TO 4342-MID-Z1 4342-MID-Z2                            
043600     MOVE SPACE TO GODK-STATUSKODER                                       
043700     CALL CBLTDLI USING ISRT                                              
043800                          ALT-PCB                                         
043900                          4342-MID-IO-AREA                                
044000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
044100     PERFORM IMS-STATUSKONTROLL                                           
044200     .                                                                    
044300     EJECT                                                                
044301 IMS-INSERT-ALT-MSG-4333 SECTION.                                         
044302     MOVE LOW-VALUE TO 4333-MID-Z1 4333-MID-Z2                            
044303     MOVE SPACE TO GODK-STATUSKODER                                       
044304     CALL CBLTDLI USING ISRT                                              
044305                          4333-PCB                                        
044306                          4333-MID-IO-AREA                                
044307     MOVE 4333-STATUS-CODE TO STATUS-WS                                   
044308     PERFORM IMS-STATUSKONTROLL                                           
044309     .                                                                    
044310     EJECT                                                                
044400 IMS-GHU-E601-KVAL SECTION.                                               
044500     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
044600            DELIMITED BY SIZE INTO SSA1                                   
044700     MOVE '  ' TO GODK-STATUSKODER                                        
044800     CALL CBLTDLI USING GHU                                               
044900                          WDE6-PCB                                        
045000                          DLI-IO-AREA                                     
045100                          SSA1                                            
045200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
045300     PERFORM IMS-STATUSKONTROLL                                           
045400     SKIP3                                                                
045500     .                                                                    
045600 IMS-REPLACE-E6 SECTION.                                                  
045700     MOVE '  ' TO GODK-STATUSKODER                                        
045800     CALL CBLTDLI USING REPL                                              
045900                          WDE6-PCB                                        
046000                          DLI-IO-AREA                                     
046100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     .                                                                    
046400     EJECT                                                                
046500 IMS-GHNP-E611-KVAL SECTION.                                              
046600     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
046700            DELIMITED BY SIZE INTO SSA1                                   
046800     STRING 'WDE611  (IDKOLLI  =' W-E611-IDKOLLI-X ')'                    
046900            DELIMITED BY SIZE INTO SSA2                                   
047000     MOVE '  ' TO GODK-STATUSKODER                                        
047100     CALL CBLTDLI USING GHNP                                              
047200                          WDE6-PCB                                        
047300                          DLI-IO-AREA                                     
047400                          SSA1 SSA2                                       
047500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
047600     PERFORM IMS-STATUSKONTROLL                                           
047700     .                                                                    
047710                                                                          
047720 IMS-GU-WDB601    SECTION.                                                
047730     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
047740          DELIMITED BY SIZE INTO SSA1                                     
047750     MOVE '  ' TO GODK-STATUSKODER                                        
047760     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
047770     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
047780     PERFORM IMS-STATUSKONTROLL                                           
047790     .                                                                    
047800     EJECT                                                                
047900 IMS-STATUSKONTROLL SECTION.                                              
048000     SET STATUS-IX TO 1                                                   
048100     SEARCH GODK-STATUS AT END CALL FELLOG                                
048200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
048300     END-SEARCH                                                           
048400     .                                                                    
