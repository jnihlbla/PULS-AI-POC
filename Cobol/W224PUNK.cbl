000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W224PUNK.                                                
000400 AUTHOR.         OLSSON SUSANNE.                                          
000500 DATE-WRITTEN.   12/12/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR ETT SUBPROGRAM                                     
001000*        VILKET BERÄKNAR STYRVÄRDEN (PUNKTER) SOM KRÄVS                   
001100*        FÖR ATT UTFÄRDA BESTÄLLNINGAR OCH LEVERANSPLANER.                
001200*        KOMMUNIKATION MED HUVUDPROGRAMMET SKER MED                       
001300*        LINK-AREA                                                        
001400*        BERÄKNADE VECKOBEHOV FÖR ARTIKELN FINNS I                        
001500*        LNK2-AREA. SE MOTSVARIGT PGM FÖR CDC: W221PUNK                   
001600*                                                                         
001700*        FÖLJANDE 3 PUNKTER BERÄKNAS I W271REFL I REFILLEN OCH            
001800*        SKALL ANVÄNDAS I ANSKAFFNINGEN.                                  
001900*          - ÖVERLAGERPUNKT = KVREFOVL                                    
002000*          - SÄKERHETSLAGER = KVREFPKT = KVSLAGER (FÖR ANSK.)             
002100*          - PÅFYLLNADSKVANT(HEMTAGN.KVANT KVQ) KVREFBER = KVEOQ          
002200*          - PÅFYLLNADSKVANT(HEMTAGNINGSKVANT KVQ) = KVREFBER             
002300*            KVANTANPASSAD MED KVPALL OCH KVULOAD.                        
002400*                                                                         
002500*                                                                         
002600*                                                                         
002700*    ABENDKODER:                                                          
002800*        U0016 -  . . . .                                                 
002900*        U1000 -  . . . .                                                 
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W224PUNK'.            
004600 77  YES                         PIC X       VALUE 'Y'.                   
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  INDX                        PIC 9(3)    VALUE ZERO.                  
005000 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
005100 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
005200 77  TEST1-YYMMDD                PIC 9(6)    VALUE ZERO.                  
005300 77  TEST2-YYMMDD                PIC 9(6)    VALUE ZERO.                  
005400     EJECT                                                                
005500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM.                                       
005700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006000     EJECT                                                                
006100                                                                          
006200 01  W-ARBETSAREOR-EOQ.                                                   
006300     03  WS-KVULOAD              PIC S9(7)       COMP-3.                  
006400     03  MULT                    PIC S9(7)       COMP-3.                  
006500     03  LAGSTA                  PIC S9(7)       COMP-3.                  
006600     03  HOGSTA                  PIC S9(7)       COMP-3.                  
006700                                                                          
006800     EJECT                                                                
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
007500     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL ABEND                                            
007800                                                                          
007900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008200     SKIP2                                                                
008300*    --- PARAMETRAR TILL W271REFL                                         
008400*01  -COPY W271REFL                                                       
008500     EJECT                                                                
008600*    --- PARAMETRAR TILL W271UTUP                                         
008700*01  -COPY W271UTUP                                                       
008800     EJECT                                                                
008900     SKIP2                                                                
009000 01  FELTEXT.                                                             
009100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009300     EJECT                                                                
009400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009500*                                                                         
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009800     SKIP3                                                                
009900 01  NYCKLAR-TILL-DLI.                                                    
010000     03  W-IDLEVNR-X.                                                     
010100         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
010200     SKIP2                                                                
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS                   PIC XX.                                  
010500     88  SEGMENT-FINNS                       VALUE '  '.                  
010600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010800     SKIP2                                                                
010900 01  GODK-STATUSKODER.                                                    
011000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011100     SKIP3                                                                
011200 01  SSA1                        PIC X(64).                               
011300 01  SSA2                        PIC X(64).                               
011400     EJECT                                                                
011500*    --- IMS FUNKTIONSKODER                                               
011600*01  -COPY W0003                                                          
011700     EJECT                                                                
011800*    ---  DLI INPUT-OUTPUT AREA                                           
011900     EJECT                                                                
012000 LINKAGE SECTION.                                                         
012100                                                                          
012200*01  -COPY W224PUNK                                                       
012300                                                                          
012400     EJECT                                                                
012500*01  -COPY W0008  -PRE REFL-2501-                                         
012600     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012800*01  -COPY W0008  -PRE REFL-WDB6-                                         
012900     05  FILLER                  PIC X.                                   
013000     EJECT                                                                
013100*01  -COPY W0008  -PRE REFL-WDK7-                                         
013200     05  FILLER                  PIC X.                                   
013300     EJECT                                                                
013400 01  REFL1-UTIL-WDK6-PCB         PIC X.                                   
013500 01  REFL1-UTIL-WDK7-PCB         PIC X.                                   
013600 01  REFL1-UTIL-WDB6-PCB         PIC X.                                   
013700*****W271UTUP**********                                                   
013800 01  UTUP1-WDK7-PCB              PIC X.                                   
013900 01  UTUP1-WDB6-PCB              PIC X.                                   
014000 01  UTUP1-UTIL-WDK6-PCB         PIC X.                                   
014100 01  UTUP1-UTIL-WDK7-PCB         PIC X.                                   
014200 01  UTUP1-UTIL-WDB6-PCB         PIC X.                                   
014300     EJECT                                                                
014400 PROCEDURE DIVISION  USING   PUNK-W224PUNK REFL-2501-PCB                  
014500                             REFL-WDB6-PCB REFL-WDK7-PCB                  
014600                             REFL1-UTIL-WDK6-PCB                          
014700                             REFL1-UTIL-WDK7-PCB                          
014800                             REFL1-UTIL-WDB6-PCB                          
014900                             UTUP1-WDK7-PCB                               
015000                             UTUP1-WDB6-PCB                               
015100                             UTUP1-UTIL-WDK6-PCB                          
015200                             UTUP1-UTIL-WDK7-PCB                          
015300                             UTUP1-UTIL-WDB6-PCB                          
015400                             .                                            
015500                                                                          
015600 MAIN SECTION.                                                            
015700                                                                          
015800                                                                          
015900     PERFORM A-INIT                                                       
016000                                                                          
016100     PERFORM B-BERAKNA-REFILLPUNKTER                                      
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 A-INIT SECTION.                                                          
016800     MOVE 'A-INIT    '     TO CURRENT-SECTION                             
016900                                                                          
017000     ACCEPT DAGENS-DATUM  FROM DATE                                       
017100     .                                                                    
017200     EJECT                                                                
017300 B-BERAKNA-REFILLPUNKTER   SECTION.                                       
017400     MOVE 'B-BERAKNA-REFILLPUNKTER '  TO CURRENT-SECTION                  
017500                                                                          
017600     INITIALIZE REFL-W271REFL                                             
017700                                                                          
017800     MOVE PUNK-IDDC         TO REFL-IDDC                                  
017900     MOVE PUNK-IDARTNR      TO REFL-IDARTNR                               
018000                                                                          
018100*-- LÄGGER MAN SPACE HÄR (NDC-CN) - TAS INGA LEDTIDER MED I W271RE        
018200*-- PUNK-IDDC-REF = SPACE FÖR LOCAL SOURCED PARTS.                        
018300     MOVE PUNK-IDDC-REF     TO REFL-IDDC-REF                              
018400                                                                          
018500     MOVE PUNK-IDREFTAB     TO REFL-IDREFTAB                              
018600     MOVE PUNK-FLWILSON     TO REFL-FLWILSON                              
018700                                                                          
018800*-- KINA SKALL ANVÄNDA MATERIALPRIS ISTÄLLET FÖR BESTÄLLNINGSPRIS.        
018900*-- SAMMA SAK FÖR NDC USA MED LOCAL SOURCING, 2017                        
019000     MOVE PUNK-PRMATRL      TO REFL-PRARTBES                              
019100                                                                          
019200*                                                                         
019300*--- KVPB-TOT = KVPB-REF + KVPBREOI VID BERÄKNING I W271REFL              
019400*                                                                         
019500     MOVE PUNK-TIREFPKT-IN  TO TEST1-YYMMDD                               
019600     MOVE DAGENS-DATUM      TO TEST2-YYMMDD                               
019700     IF  TEST1-YYMMDD  >= TEST2-YYMMDD                                    
019800*                                                                         
019900*----  MANUELL PÅFYLLNADSPUNKT GÄLLER                                     
020000*                                                                         
020100       MOVE PUNK-KVREFPKT-IN   TO REFL-IN-KVREFPKT                        
020200       MOVE PUNK-TIREFPKT-IN   TO PUNK-TIREFPKT                           
020300     ELSE                                                                 
020400       MOVE ZERO               TO REFL-IN-KVREFPKT                        
020500                                                                          
020600       MOVE ZERO               TO PUNK-TIREFPKT                           
020700     END-IF                                                               
020800                                                                          
020900     MOVE PUNK-TIREFPAF-IN     TO TEST1-YYMMDD                            
021000     MOVE DAGENS-DATUM         TO TEST2-YYMMDD                            
021100     IF TEST1-YYMMDD >= TEST2-YYMMDD                                      
021200*                                                                         
021300*----  MANUELL PÅFYLLNADSKVANTITET ÄR SATT                                
021400*                                                                         
021500       MOVE PUNK-KVREFBER-IN   TO REFL-IN-KVREFBER                        
021600       MOVE PUNK-TIREFPAF-IN   TO PUNK-TIREFPAF                           
021700     ELSE                                                                 
021800       MOVE ZERO               TO REFL-IN-KVREFBER                        
021900                                                                          
022000       MOVE ZERO               TO PUNK-TIREFPAF                           
022100     END-IF                                                               
022200                                                                          
022300     MOVE PUNK-IDLEVNR         TO REFL-IN-IDLEVNR-DC                      
022400     MOVE ZERO                 TO REFL-NDC-KVDAGAR-TBT-DC                 
022500                                                                          
022600     MOVE +1  TO INDX                                                     
022700     PERFORM UNTIL INDX > 12                                              
022800       MOVE PUNK-RESEASON(INDX)                                           
022900                               TO REFL-RESEASON(INDX)                     
023000       ADD +1  TO INDX                                                    
023100     END-PERFORM                                                          
023200                                                                          
023300     MOVE PUNK-FLFLYG          TO REFL-FLFLYG                             
023400                                                                          
023500***  GET LEADTIME DEMAND FROM W271UTUP                                    
023600*                                                                         
023700     INITIALIZE UTUP-W271UTUP                                             
023800     MOVE 004                  TO UTUP-KDCALL                             
023900     MOVE PUNK-IDARTNR         TO UTUP-IDARTNR                            
024000     MOVE PUNK-IDDC            TO UTUP-IDDC                               
024100     MOVE PUNK-IDDC-REF        TO UTUP-IDDC-REF                           
024200                                                                          
024300     CALL W271UTUP USING UTUP-W271UTUP                                    
024400                         UTUP1-WDK7-PCB                                   
024500                         UTUP1-WDB6-PCB                                   
024600                         UTUP1-UTIL-WDK6-PCB                              
024700                         UTUP1-UTIL-WDK7-PCB                              
024800                         UTUP1-UTIL-WDB6-PCB                              
024900     IF UTUP-KDSVAR-OK                                                    
025000        MOVE UTUP-LEADTID-BEHOV                                           
025100                               TO REFL-IN-LEADTID-BEHOV                   
025200     ELSE                                                                 
025300        DISPLAY 'W271UTUP-ERROR :' UTUP-TEXT                              
025400        CALL FELLOG                                                       
025500     END-IF                                                               
025600                                                                          
025700                                                                          
025800     CALL W271REFL USING REFL-W271REFL REFL-2501-PCB                      
025900                         REFL-WDB6-PCB REFL-WDK7-PCB                      
026000                         REFL1-UTIL-WDK6-PCB                              
026100                         REFL1-UTIL-WDK7-PCB                              
026200                         REFL1-UTIL-WDB6-PCB                              
026300                                                                          
026400                                                                          
026500*                                                                         
026600*----  TABELLENS KVREFBER-VÄRDE = KVEOQ FÖR ANSKAFFNINGEN.                
026700*                                                                         
026800*----  KVANTANPASSAD Q-KVANT KVREFBER =                                   
026900*----  (KVEOQ) KVREFBER AVRUNDAT MED KVPALL + KVULOAD                     
027000*                                                                         
027100     MOVE REFL-KVREFBER        TO PUNK-KVREFBER                           
027200                                  PUNK-KVEOQ                              
027300                                                                          
027400     IF PUNK-TIREFPAF = ZERO                                              
027500        PERFORM BA-AVRUNDA-KVREFBER                                       
027600     END-IF                                                               
027700                                                                          
027800     PERFORM BB-AVRUNDA-KVULOAD                                           
027900                                                                          
028000                                                                          
028100*                                                                         
028200*----  TABELLENS KVREFPKT = KVSLAGER FÖR ANSKAFFNINGEN.                   
028300*                                                                         
028400     MOVE REFL-KVREFPKT        TO PUNK-KVREFPKT                           
028500                                                                          
028600     PERFORM BC-BERAKNA-KVSLAGER                                          
028700                                                                          
028800                                                                          
028900     MOVE REFL-KVREFOVL        TO PUNK-KVREFOVL                           
029000                                                                          
029100                                                                          
029200     .                                                                    
029300     EJECT                                                                
029400 BA-AVRUNDA-KVREFBER  SECTION.                                            
029500     MOVE 'BA-AVRUNDA-KVREFBER  '  TO CURRENT-SECTION                     
029600                                                                          
029700     IF  PUNK-KVPALL > ZERO                                               
029800       COMPUTE PUNK-KVREFBER = PUNK-KVREFBER / PUNK-KVPALL                
029900       COMPUTE PUNK-KVREFBER = PUNK-KVPALL * PUNK-KVREFBER                
030000       IF PUNK-KVREFBER = ZERO                                            
030100          MOVE PUNK-KVPALL  TO PUNK-KVREFBER                              
030200       END-IF                                                             
030300     END-IF                                                               
030400                                                                          
030500     .                                                                    
030600     EJECT                                                                
030700 BB-AVRUNDA-KVULOAD   SECTION.                                            
030800     MOVE 'BB-AVRUNDA-KVULOAD   '     TO CURRENT-SECTION                  
030900******************************************************************        
031000*                                                                *        
031100*    OM KVULOAD ÄR NOLL SÄTTS KVULOAD TILL KVPALL                *        
031200*                                                                *        
031300******************************************************************        
031400                                                                          
031500     IF PUNK-KVULOAD = ZERO                                               
031600        MOVE PUNK-KVPALL  TO WS-KVULOAD                                   
031700     ELSE                                                                 
031800        MOVE PUNK-KVULOAD TO WS-KVULOAD                                   
031900     END-IF                                                               
032000                                                                          
032100     IF PUNK-KVREFBER < WS-KVULOAD                                        
032200        MOVE WS-KVULOAD   TO PUNK-KVREFBER                                
032300     END-IF                                                               
032400                                                                          
032500     IF PUNK-KVREFBER > WS-KVULOAD AND WS-KVULOAD > ZERO                  
032600        PERFORM BBA-ROUND-TO-KVULOAD                                      
032700     END-IF                                                               
032800                                                                          
032900     .                                                                    
033000     EJECT                                                                
033100 BBA-ROUND-TO-KVULOAD   SECTION.                                          
033200     MOVE 'PA-ROUND-TO-KVULOAD '   TO CURRENT-SECTION                     
033300******************************************************************        
033400*                                                                *        
033500*    AVRUNDA KVREFBER TILL NÄRMASTE KVULOAD                      *        
033600*                                                                *        
033700******************************************************************        
033800                                                                          
033900     COMPUTE MULT   = PUNK-KVREFBER / WS-KVULOAD                          
034000     COMPUTE LAGSTA = WS-KVULOAD * MULT                                   
034100     COMPUTE HOGSTA = WS-KVULOAD * (MULT + 1)                             
034200                                                                          
034300     IF (HOGSTA - PUNK-KVREFBER) > (PUNK-KVREFBER - LAGSTA)               
034400        MOVE LAGSTA    TO PUNK-KVREFBER                                   
034500        IF PUNK-KVREFBER < PUNK-KVPALL                                    
034600           MOVE HOGSTA TO PUNK-KVREFBER                                   
034700        END-IF                                                            
034800     ELSE                                                                 
034900        MOVE HOGSTA    TO PUNK-KVREFBER                                   
035000     END-IF                                                               
035100                                                                          
035200     .                                                                    
035300     EJECT                                                                
035400 BC-BERAKNA-KVSLAGER   SECTION.                                           
035500     MOVE 'BC-BERAKNA-KVSLAGER '   TO CURRENT-SECTION                     
035600                                                                          
035700     MOVE PUNK-TIMANSEC-IN  TO TEST1-YYMMDD                               
035800     MOVE DAGENS-DATUM      TO TEST2-YYMMDD                               
035900     IF  TEST1-YYMMDD  >= TEST2-YYMMDD                                    
036000*                                                                         
036100*----  MANUELLT SÄKERHETSLAGER GÄLLER                                     
036200*                                                                         
036300       MOVE PUNK-KVSLAGER-IN   TO PUNK-KVSLAGER                           
036400       MOVE PUNK-TIMANSEC-IN   TO PUNK-TIMANSEC                           
036500     ELSE                                                                 
036600       MOVE REFL-KVREFPKT      TO PUNK-KVSLAGER                           
036700       MOVE ZERO               TO PUNK-TIMANSEC                           
036800     END-IF                                                               
036900                                                                          
037000     .                                                                    
037100     EJECT                                                                
