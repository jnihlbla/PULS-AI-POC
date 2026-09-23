000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0070800.                                                
000300 AUTHOR.         MATS VINNEFORS.                                          
000400 DATE-WRITTEN.   FEBRUARI 1984.                                           
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*    FUNKTION.   TP-UPPDATERINGSPROGRAM. LISTAR JOBB INGÅENDE             
000800*                I RUTIN, SUBMITTAR JOBB FRÅN ANGIVEN RUTIN ELLER         
000900*                DELETAR GIVET JOBB FRÅN GIVEN RUTIN.                     
001000     SKIP2                                                                
001100*    INDATA.                                                              
001200*        TRANSAKTION: W0T708                                              
001300*        MID:         W0I70801                                            
001400*    UTDATA.                                                              
001500*        MOD:         W0O70801     OM INDATA FEL                          
001600*                     W0O70901     OM INDATA RÄTT                         
001700*    SUBPROGRAM.                                                          
001800*        FELLOG                                                           
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP3                                                                
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(8)    VALUE 'W0070800'.            
002800 77  JA                          PIC X(1)    VALUE 'J'.                   
002900 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003000 77  ALT-ISRT                    PIC X(1)    VALUE 'N'.                   
003100 77  OK                          PIC X(1)    VALUE 'O'.                   
003200 77  FEL                         PIC X(1)    VALUE 'F'.                   
003300 77  SECURITY-TEST               PIC X(1)    VALUE 'F'.                   
003400 77  MAX-RADER                   PIC S9(3)   VALUE +12  COMP-3.           
003500 77  MIN-MOD-LAENGD              PIC S9(4)   VALUE +49  COMP SYNC.        
003600 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +882 COMP SYNC.        
003700 77  IX                          PIC S9(9)   VALUE ZERO COMP SYNC.        
003800                                                                          
003900 01  DYNAMISKA-SUBPROGRAM.                                                
004000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004200                                                                          
004300 01  W-IDTRANS                   PIC X(4).                                
004400     88  EGEN-BILD                           VALUE '0708'.                
004500     SKIP2                                                                
004600 01  FUNKTIONS-TEST              PIC X(1).                                
004700     88  FUNKTION-OK                         VALUE 'O'.                   
004800     SKIP2                                                                
004900 01  W-TIUPPTID                  PIC 9(9).                                
005000                                                                          
005100 01  FILLER REDEFINES W-TIUPPTID.                                         
005200     03  FILLER                  PIC 9(1).                                
005300     03  W-TIUPPTID-2-7          PIC 9(6).                                
005400     03  FILLER                  PIC 9(2).                                
005500     EJECT                                                                
005600 01  NYCKLAR-TILL-DLI.                                                    
005700     03  W-WDP101KY-6001-X.                                               
005800         05  FILLER              PIC X(4)    VALUE '6001'.                
005900         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
006000     SKIP2                                                                
006100     03  W-WDP101KY-6011-X.                                               
006200         05  FILLER              PIC X(4)    VALUE '6011'.                
006300         05  W-IDRUTIN-6011      PIC X(8).                                
006400         05  LOWVALUE            PIC X(18)   VALUE LOW-VALUE.             
006500     SKIP2                                                                
006600     03  W-WDP101KY-6021-X.                                               
006700         05  FILLER              PIC X(4)    VALUE '6021'.                
006800         05  W-IDRUTIN-6021      PIC X(8).                                
006900         05  W-IDJOB-6021        PIC X(8).                                
007000         05  LOWVALUE            PIC X(10)   VALUE LOW-VALUE.             
007100     SKIP2                                                                
007200     03  W-IDUSER-X.                                                      
007300         05  W-IDUSER            PIC X(8).                                
007400     SKIP2                                                                
007500     03  W-IDOWNER-X.                                                     
007600         05  W-IDOWNER           PIC X(8).                                
007700     SKIP2                                                                
007800     03  W-IDJOB-X.                                                       
007900         05  W-IDJOB             PIC X(8).                                
008000     EJECT                                                                
008100 01  FELMEDDELANDE.                                                       
008200                                                                          
008300     03  W-FEL-1.                                                         
008400         05  FILLER              PIC X(26)                                
008500         VALUE 'EJ AUKTORISERAD ANVÄNDARE '.                              
008600         05  FILLER              PIC X(26)                                
008700         VALUE 'UNAUTHORIZED USER         '.                              
008800     03  FILLER REDEFINES W-FEL-1.                                        
008900         05  FEL-1 OCCURS 2      PIC X(26).                               
009000                                                                          
009100     03  W-FEL-2.                                                         
009200         05  FILLER              PIC X(19)                                
009300         VALUE 'RUTIN SAKNAS       '.                                     
009400         05  FILLER              PIC X(19)                                
009500         VALUE 'ROUTINE IS MISSING '.                                     
009600     03  FILLER REDEFINES W-FEL-2.                                        
009700         05  FEL-2 OCCURS 2      PIC X(19).                               
009800                                                                          
009900     03  W-FEL-3.                                                         
010000         05  FILLER              PIC X(26)                                
010100         VALUE 'UPPLYSTA FÄLT FEL         '.                              
010200         05  FILLER              PIC X(26)                                
010300         VALUE 'HIGHLIT FIELDS ARE WRONG  '.                              
010400     03  FILLER REDEFINES W-FEL-3.                                        
010500         05  FEL-3 OCCURS 2      PIC X(26).                               
010600                                                                          
010700     03  W-FEL-4.                                                         
010800         05  FILLER              PIC X(32)                                
010900         VALUE 'BEGÄRD FUNKTION EJ UTFÖRD       '.                        
011000         05  FILLER              PIC X(32)                                
011100         VALUE 'REQUESTED FUNCTION NOT PERFORMED'.                        
011200     03  FILLER REDEFINES W-FEL-4.                                        
011300         05  FEL-4 OCCURS 2      PIC X(32).                               
011400                                                                          
011500     03  W-FEL-5.                                                         
011600         05  FILLER              PIC X(14)                                
011700         VALUE 'JOBB SAKNAS   '.                                          
011800         05  FILLER              PIC X(14)                                
011900         VALUE 'JOB IS MISSING'.                                          
012000     03  FILLER REDEFINES W-FEL-5.                                        
012100         05  FEL-5 OCCURS 2      PIC X(14).                               
012200     EJECT                                                                
012300 01  INFO-MEDDELANDE.                                                     
012400                                                                          
012500     03  W-INFO-1.                                                        
012600         05  FILLER.                                                      
012700             07  W-IDJOB-SVE     PIC X(8).                                
012800             07  FILLER          PIC X(13) VALUE ' ÄR BORTTAGET'.         
012900             07  W-IDJOB-ENG     PIC X(8).                                
013000             07  FILLER          PIC X(13) VALUE ' IS DELETED  '.         
013100     03  FILLER REDEFINES W-INFO-1.                                       
013200         05  INFO-1 OCCURS 2     PIC X(21).                               
013300                                                                          
013400     03  W-INFO-2.                                                        
013500         05  FILLER              PIC X(26)                                
013600             VALUE 'TRYCK PF11 FÖR UPPDATERING'.                          
013700         05  FILLER              PIC X(26)                                
013800             VALUE 'PRESS PF11 TO UPDATE      '.                          
013900     03  FILLER REDEFINES W-INFO-2.                                       
014000         05  INFO-2 OCCURS 2     PIC X(26).                               
014100                                                                          
014200     03  W-INFO-3.                                                        
014300         05  FILLER              PIC X(24)                                
014400             VALUE 'TRYCK PF8 FÖR NÄSTA SIDA'.                            
014500         05  FILLER              PIC X(24)                                
014600             VALUE 'PRESS PF8 FOR NEXT PAGE '.                            
014700     03  FILLER REDEFINES W-INFO-3.                                       
014800         05  INFO-3 OCCURS 2     PIC X(24).                               
014900     EJECT                                                                
015000 01  FILLER.                                                              
015100     03  TEST-BEFUNK             PIC X(8).                                
015200                                                                          
015300         88  BEFUNK-TILLATEN                 VALUE 'DELETE  '             
015400                             'RADERA  ' 'STARTA  ' 'SUBMIT  '.            
015500                                                                          
015600         88  BEFUNK-SUBMIT                   VALUE 'STARTA  '             
015700                                                   'SUBMIT  '.            
015800                                                                          
015900         88  BEFUNK-DELETE                   VALUE 'DELETE  '             
016000                                                   'RADERA  '.            
016100     EJECT                                                                
016200 01  TETRSTAT-TABELL.                                                     
016300     03  FILLER                  PIC X(15)                                
016400         VALUE 'PÅBÖRJAD       '.                                         
016500     03  FILLER                  PIC X(15)                                
016600         VALUE 'KLAR, FLERA    '.                                         
016700     03  FILLER                  PIC X(15)                                
016800         VALUE 'KLAR, EN GÅNG  '.                                         
016900     03  FILLER                  PIC X(15)                                
017000         VALUE 'STARTAD FLERA  '.                                         
017100     03  FILLER                  PIC X(15)                                
017200         VALUE 'STARTAD EN GÅNG'.                                         
017300     03  FILLER                  PIC X(15)                                
017400         VALUE 'KÖRD-OK, FLERA '.                                         
017500     03  FILLER                  PIC X(15)                                
017600         VALUE 'KÖRD-OK, EN GGR'.                                         
017700     03  FILLER                  PIC X(15)                                
017800         VALUE 'KÖRD-ABEND FLER'.                                         
017900     03  FILLER                  PIC X(15)                                
018000         VALUE 'KÖRD-ABEND EN  '.                                         
018100     03  FILLER                  PIC X(15)                                
018200         VALUE 'SKALL RENSAS   '.                                         
018300*                                                                         
018400     03  FILLER                  PIC X(15)                                
018500         VALUE 'INITIATED      '.                                         
018600     03  FILLER                  PIC X(15)                                
018700         VALUE 'READY, MANY    '.                                         
018800     03  FILLER                  PIC X(15)                                
018900         VALUE 'READY, ONE TIME'.                                         
019000     03  FILLER                  PIC X(15)                                
019100         VALUE 'STARTED, MANY  '.                                         
019200     03  FILLER                  PIC X(15)                                
019300         VALUE 'STARTED, ONE   '.                                         
019400     03  FILLER                  PIC X(15)                                
019500         VALUE 'RUN, OK  MANY  '.                                         
019600     03  FILLER                  PIC X(15)                                
019700         VALUE 'RUN, OK  ONE   '.                                         
019800     03  FILLER                  PIC X(15)                                
019900         VALUE 'RUN-ABEND  MANY'.                                         
020000     03  FILLER                  PIC X(15)                                
020100         VALUE 'RUN-ABEND  ON  '.                                         
020200     03  FILLER                  PIC X(15)                                
020300         VALUE 'WILL SCRATCH   '.                                         
020400     SKIP3                                                                
020500 01  FILLER REDEFINES TETRSTAT-TABELL.                                    
020600     03  FILLER OCCURS 2.                                                 
020700         05  FILLER OCCURS 10.                                            
020800             07  W-TETRSTAT      PIC X(15).                               
020900     EJECT                                                                
021000*                        ****    TP-AREOR                                 
021100 01  FILLER                      PIC X(16)   VALUE ' MFS-WS   '.          
021200     SKIP2                                                                
021300*01  MID -COPY W0I70801                                                   
021400     EJECT                                                                
021500*01  -COPY WMSGAREA                                                       
021600     EJECT                                                                
021700*    03  MOD -COPY W0O70801 -RED MSG-AREA.                                
021800     EJECT                                                                
021900*01  MOD -COPY W0I70901 -PRE MOD-.                                        
022000     EJECT                                                                
022100*01  -COPY WMFSAREA.                                                      
022200     EJECT                                                                
022300******************************************************************        
022400*****                                                                     
022500*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022600*****                                                                     
022700 01  IMS-WS.                                                              
022800     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
022900     SKIP3                                                                
023000*****                    **** STATUS-KOD FRÅN IMS                         
023100     03  STATUS-WS               PIC X(2).                                
023200         88  SEGMENT-FINNS                   VALUE '  '.                  
023300         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
023400     SKIP3                                                                
023500*****                    **** SEG-LEVEL FRÅN IMS                          
023600     03  LEVEL-WS                PIC X(2).                                
023700         88  SECURITY-SEG-SAKNAS             VALUE '01'.                  
023800     SKIP3                                                                
023900     03  GODK-STATUSKODER.                                                
024000         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
024100     SKIP3                                                                
024200 01  SSA1                        PIC X(64).                               
024300 01  SSA2                        PIC X(64).                               
024400     EJECT                                                                
024500*                            IMS FUNKTIONSKODER                           
024600*01  -COPY W0003                                                          
024700     EJECT                                                                
024800*                            DLI INPUT-OUTPUT AREA                        
024900 01  DLI-IO-AREA.                                                         
025000     03  IO-AREA                 PIC X(100)  VALUE SPACE.                 
025100     SKIP3                                                                
025200*    03  WLJCLC01  -COPY WDP101  -RED IO-AREA.                            
025300     EJECT                                                                
025400*    03  WLJCLD11  -COPY WDP111  -RED IO-AREA.                            
025500     EJECT                                                                
025600*    03  WLJCLC12  -COPY WDP113  -RED IO-AREA.                            
025700     EJECT                                                                
025800 LINKAGE SECTION.                                                         
025900     SKIP2                                                                
026000*01  -COPY W0009     -PRE MSG-                                            
026100     EJECT                                                                
026200*01  -COPY W0009     -PRE ALT-                                            
026300     EJECT                                                                
026400*01  -COPY W0008     -PRE JCLA-                                           
026500         05  FILLER              PIC X.                                   
026600     EJECT                                                                
026700*01  -COPY W0008     -PRE JCLC-                                           
026800         05  FILLER              PIC X.                                   
026900     EJECT                                                                
027000*01  -COPY W0008     -PRE JCLD-                                           
027100         05  FILLER              PIC X.                                   
027200     EJECT                                                                
027300 PROCEDURE DIVISION USING MSG-PCB ALT-PCB JCLA-PCB                        
027400                                  JCLC-PCB JCLD-PCB.                      
027500 MAIN SECTION.                                                            
027600     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB JCLA-PCB                       
027700                                   JCLC-PCB JCLD-PCB.                     
027800                                                                          
027900     PERFORM IMS-GET-MSG                                                  
028000     IF SEGMENT-FINNS                                                     
028100         PERFORM A-INIT-SPARA-INPUT                                       
028200         PERFORM B-KOLLA-FUNKTION                                         
028300         IF FUNKTION-OK                                                   
028400             PERFORM IMS-GET-6011-ROT                                     
028500             IF SEGMENT-FINNS                                             
028600                 PERFORM C-TESTA-SECURITY                                 
028700                 IF SECURITY-TEST = OK                                    
028800                     IF BEFUNK-SUBMIT                                     
028900                         PERFORM D-SPARKA-IGANG-W0T709                    
029000                         PERFORM IMS-INSERT-ALT-MSG                       
029100                         MOVE JA TO ALT-ISRT                              
029200                     ELSE                                                 
029300                         IF BEFUNK-DELETE                                 
029400                             PERFORM E-DELETE-JOB                         
029500                         END-IF                                           
029600                         PERFORM F-VISA-RUTIN                             
029700                     END-IF                                               
029800                 END-IF                                                   
029900             ELSE                                                         
030000                 MOVE FEL-2 (IX) TO MOD-TEMFSFEL                          
030100             END-IF                                                       
030200         END-IF                                                           
030300         IF ALT-ISRT = NEJ                                                
030400             PERFORM IMS-INSERT-MSG                                       
030500         END-IF                                                           
030600     END-IF                                                               
030700                                                                          
030800     MOVE ZERO TO RETURN-CODE                                             
030900     GOBACK                                                               
031000     .                                                                    
031100     EJECT                                                                
031200 A-INIT-SPARA-INPUT SECTION.                                              
031300     SKIP2                                                                
031400     IF MSG-DUBBLA-TRANSKODER                                             
031500         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I70801               
031600         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS W-IDTRANS                      
031700         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
031800         IF MSG-KDTRANS-1  = 'W0T708U '                                   
031900            MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                      
032000         END-IF                                                           
032100     ELSE                                                                 
032200         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I70801                
032300         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS W-IDTRANS                      
032400         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
032500         MOVE SPACE TO MFS-KDTRTYP                                        
032600     END-IF                                                               
032700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
032800     MOVE LOW-VALUE TO MOD-W0O70801                                       
032900     MOVE 'W0O70801' TO MFS-IDMOD                                         
033000     MOVE '0708' TO MOD-IDTRANS                                           
033100     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
033200                                                                          
033300     IF MID-IDRUTIN-IN = ALL '+'                                          
033400         MOVE MID-IDRUTIN-UT TO W-IDRUTIN-6011                            
033500                                W-IDRUTIN-6021 MOD-IDRUTIN-UT             
033600     ELSE                                                                 
033700         MOVE MID-IDRUTIN-IN TO W-IDRUTIN-6011                            
033800                                W-IDRUTIN-6021 MOD-IDRUTIN-UT             
033900     END-IF                                                               
034000                                                                          
034100     IF W-IDRUTIN-6021 = 'W31527HR'                                       
034200       MOVE 'W31527HW' TO W-IDRUTIN-6021                                  
034300     END-IF                                                               
034400     IF W-IDRUTIN-6021 = 'W31530HR'                                       
034500       MOVE 'W31530HW' TO W-IDRUTIN-6021                                  
034600     END-IF                                                               
034700     IF W-IDRUTIN-6021 = 'W31535HR'                                       
034800       MOVE 'W31535HW' TO W-IDRUTIN-6021                                  
034900     END-IF                                                               
035000     IF W-IDRUTIN-6021 = 'W31540HR'                                       
035100       MOVE 'W31540HW' TO W-IDRUTIN-6021                                  
035200     END-IF                                                               
035300     IF W-IDRUTIN-6021 = 'W31545HR'                                       
035400       MOVE 'W31545HW' TO W-IDRUTIN-6021                                  
035500     END-IF                                                               
035600                                                                          
035700     IF MFS-UPDATE                                                        
035800         MOVE MID-IDJOB-AKTIV TO W-IDJOB W-IDJOB-6021                     
035900         MOVE MID-IDJOB-UT TO MOD-IDJOB-UT                                
036000     ELSE                                                                 
036100         IF MFS-IDPFK = '7'                                               
036200             MOVE SPACE TO W-IDJOB                                        
036300         ELSE                                                             
036400             IF MID-IDJOB-IN NOT = ALL '+'                                
036500                 MOVE MID-IDJOB-IN TO W-IDJOB MOD-IDJOB-UT                
036600             ELSE                                                         
036700                 IF EGEN-BILD                                             
036800                     IF MID-IDJOB-SKIP = SPACE                            
036900                         MOVE MID-IDJOB-UT TO W-IDJOB MOD-IDJOB-UT        
037000                     ELSE                                                 
037100                         MOVE MID-IDJOB-SKIP TO W-IDJOB                   
037200                     END-IF                                               
037300                 ELSE                                                     
037400                     MOVE MID-IDJOB-UT TO W-IDJOB MOD-IDJOB-UT            
037500                 END-IF                                                   
037600             END-IF                                                       
037700         END-IF                                                           
037800     END-IF                                                               
037900                                                                          
038000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
038100                             MOD-TEMFSINF                                 
038200                             MOD-IDRUTIN-IN                               
038300                             MOD-IDJOB-IN                                 
038400     MOVE MFS-ROER-EJ-FAELT TO MOD-BEFUNK                                 
038500                               MOD-IDJOB-AKTIV                            
038600     IF EGEN-BILD                                                         
038700         SET MOD-IX-LINE TO +1                                            
038800         PERFORM UNTIL MOD-IX-LINE > MAX-RADER - 1                        
038900             MOVE MFS-ROER-EJ-FAELT TO MOD-IDJOB (MOD-IX-LINE)            
039000                                       MOD-TIREGDAT (MOD-IX-LINE)         
039100                                       MOD-TIUPPDAT (MOD-IX-LINE)         
039200                                       MOD-TIUPPTID (MOD-IX-LINE)         
039300                                       MOD-KDTRSTAT (MOD-IX-LINE)         
039400                                       MOD-TETRSTAT (MOD-IX-LINE)         
039500                                       MOD-BEJOB (MOD-IX-LINE)            
039600             SET MOD-IX-LINE UP BY +1                                     
039700         END-PERFORM                                                      
039800     END-IF                                                               
039900                                                                          
040000     IF ENGLISH-TEXT                                                      
040100         MOVE 2 TO IX                                                     
040200         MOVE 'N' TO MFS-KDHUVOMR                                         
040300     ELSE                                                                 
040400         MOVE 1 TO IX                                                     
040500     END-IF                                                               
040600     .                                                                    
040700     EJECT                                                                
040800 B-KOLLA-FUNKTION SECTION.                                                
040900     SKIP2                                                                
041000     MOVE OK TO FUNKTIONS-TEST                                            
041100                                                                          
041200     IF MFS-UPDATE                                                        
041300         MOVE MID-BEFUNK TO TEST-BEFUNK                                   
041400                                                                          
041500         IF BEFUNK-TILLATEN                                               
041600             MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEFUNK-ATTR                 
041700         ELSE                                                             
041800             MOVE MFS-ALFA-FAELT-FEL TO MOD-BEFUNK-ATTR                   
041900             MOVE FEL-3 (IX) TO MOD-TEMFSFEL                              
042000             MOVE FEL TO FUNKTIONS-TEST                                   
042100         END-IF                                                           
042200                                                                          
042300         IF MID-IDJOB-AKTIV NOT = ALL '+'                                 
042400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDJOB-AKTIV-ATTR            
042500         ELSE                                                             
042600             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDJOB-AKTIV-ATTR              
042700             MOVE FEL-3 (IX) TO MOD-TEMFSFEL                              
042800             MOVE FEL TO FUNKTIONS-TEST                                   
042900         END-IF                                                           
043000     ELSE                                                                 
043100                                                                          
043200         IF (MID-BEFUNK NOT = ALL '+' OR                                  
043300                 MID-IDJOB-AKTIV NOT = ALL '+') AND EGEN-BILD             
043400             MOVE MFS-ALFA-FAELT-FEL TO MOD-BEFUNK-ATTR                   
043500                                        MOD-IDJOB-AKTIV-ATTR              
043600             MOVE FEL-4 (IX) TO MOD-TEMFSFEL                              
043700             MOVE INFO-2 (IX) TO MOD-TEMFSINF                             
043800             MOVE FEL TO FUNKTIONS-TEST                                   
043900         END-IF                                                           
044000     END-IF                                                               
044100     .                                                                    
044200     EJECT                                                                
044300 C-TESTA-SECURITY SECTION.                                                
044400     SKIP2                                                                
044500     MOVE MSG-SIGNON-USERID TO W-IDOWNER W-IDUSER                         
044600     MOVE FEL TO SECURITY-TEST                                            
044700     IF MFS-UPDATE                                                        
044800         IF BEFUNK-SUBMIT OR BEFUNK-DELETE                                
044900            PERFORM IMS-GET-6021-IDUSER-KNTL                              
045000            IF SEGMENT-FINNS                                              
045100               MOVE OK TO SECURITY-TEST                                   
045200            ELSE                                                          
045300               PERFORM IMS-GET-6011-IDUSER-KNTL                           
045400               IF SEGMENT-FINNS                                           
045500                  MOVE OK TO SECURITY-TEST                                
045600               END-IF                                                     
045700            END-IF                                                        
045800         END-IF                                                           
045900                                                                          
046000         IF SECURITY-TEST = FEL                                           
046100             IF SECURITY-SEG-SAKNAS                                       
046200                 PERFORM IMS-GET-6001-KNTL                                
046300                 IF SEGMENT-FINNS                                         
046400                     MOVE OK TO SECURITY-TEST                             
046500                 ELSE                                                     
046600                     MOVE FEL-1 (IX) TO MOD-TEMFSFEL                      
046700                 END-IF                                                   
046800             ELSE                                                         
046900                 MOVE FEL-5 (IX) TO MOD-TEMFSFEL                          
047000             END-IF                                                       
047100         END-IF                                                           
047200     ELSE                                                                 
047300         MOVE OK TO SECURITY-TEST                                         
047400     END-IF                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 D-SPARKA-IGANG-W0T709 SECTION.                                           
047800     SKIP2                                                                
047900     MOVE W-IDRUTIN-6011 TO MOD-MID-IDRUTIN-IN                            
048000     MOVE W-IDJOB TO MOD-MID-IDJOB-IN                                     
048100                                                                          
048200     MOVE MIN-MOD-LAENGD TO MSG-KVLL                                      
048300     MOVE 'W0T709U ' TO MSG-KDTRANS-1                                     
048400     MOVE '0708' TO MSG-IDTRANS-1                                         
048500     MOVE IX TO MSG-KDMFSFOR-1                                            
048600     MOVE MOD-MID-W0I70901 TO MSG-INDATA-MINUS-1-TRANSKOD                 
048700     .                                                                    
048800     EJECT                                                                
048900 E-DELETE-JOB SECTION.                                                    
049000     SKIP2                                                                
049100     PERFORM IMS-GET-HOLD-6011-JOB                                        
049200     PERFORM IMS-DELETE-6011-JOB                                          
049300     PERFORM IMS-GET-HOLD-6021                                            
049400     PERFORM IMS-DELETE-6021                                              
049500                                                                          
049600     MOVE W-IDJOB TO W-IDJOB-SVE                                          
049700                     W-IDJOB-ENG                                          
049800     MOVE INFO-1 (IX) TO MOD-TEMFSINF                                     
049900     MOVE MFS-FORMATETS-ATTR TO MOD-BEFUNK-ATTR                           
050000                                MOD-IDJOB-AKTIV-ATTR                      
050100     MOVE MFS-RENSA-FAELT TO MOD-BEFUNK                                   
050200                             MOD-IDJOB-AKTIV                              
050300     .                                                                    
050400     EJECT                                                                
050500 F-VISA-RUTIN SECTION.                                                    
050600     SKIP2                                                                
050700     IF MFS-UPDATE                                                        
050800         MOVE MID-IDJOB-UT TO W-IDJOB                                     
050900     END-IF                                                               
051000                                                                          
051100     PERFORM IMS-GET-FIRST-6011-JOB                                       
051200     SET MOD-IX-LINE TO +1                                                
051300     PERFORM UNTIL MOD-IX-LINE > MAX-RADER - 1                            
051400         IF SEGMENT-FINNS                                                 
051500             MOVE JOB-IDJOB TO MOD-IDJOB (MOD-IX-LINE)                    
051600             MOVE JOB-TIREGDAT TO MOD-TIREGDAT (MOD-IX-LINE)              
051700             MOVE JOB-TIUPPDAT TO MOD-TIUPPDAT (MOD-IX-LINE)              
051800             MOVE JOB-TIUPPTID TO W-TIUPPTID                              
051900             MOVE W-TIUPPTID-2-7 TO MOD-TIUPPTID (MOD-IX-LINE)            
052000             MOVE JOB-KDTRSTAT TO MOD-KDTRSTAT (MOD-IX-LINE)              
052100             ADD 1 TO JOB-KDTRSTAT                                        
052200             MOVE W-TETRSTAT (IX, JOB-KDTRSTAT) TO                        
052300                                     MOD-TETRSTAT (MOD-IX-LINE)           
052400             MOVE JOB-BEJOB TO MOD-BEJOB (MOD-IX-LINE)                    
052500             PERFORM IMS-GET-6011-JOB                                     
052600         ELSE                                                             
052700             MOVE MFS-RENSA-FAELT TO MOD-IDJOB (MOD-IX-LINE)              
052800                                     MOD-TIREGDAT (MOD-IX-LINE)           
052900                                     MOD-TIUPPDAT (MOD-IX-LINE)           
053000                                     MOD-TIUPPTID (MOD-IX-LINE)           
053100                                     MOD-KDTRSTAT (MOD-IX-LINE)           
053200                                     MOD-TETRSTAT (MOD-IX-LINE)           
053300                                     MOD-BEJOB (MOD-IX-LINE)              
053400         END-IF                                                           
053500         SET MOD-IX-LINE UP BY +1                                         
053600     END-PERFORM                                                          
053700                                                                          
053800     IF SEGMENT-FINNS AND MOD-IX-LINE = MAX-RADER AND                     
053900            NOT MFS-UPDATE                                                
054000         MOVE INFO-3 (IX) TO MOD-TEMFSINF                                 
054100         MOVE JOB-IDJOB TO MOD-IDJOB-SKIP                                 
054200     END-IF                                                               
054300     .                                                                    
054400     EJECT                                                                
054500* IMS SEKTIONER                                                           
054600     SKIP3                                                                
054700 IMS-GET-MSG SECTION.                                                     
054800     SKIP2                                                                
054900     MOVE '  QC' TO GODK-STATUSKODER                                      
055000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
055100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
055200     PERFORM IMS-STATUSKONTROLL                                           
055300     .                                                                    
055400     SKIP3                                                                
055500 IMS-INSERT-MSG SECTION.                                                  
055600     SKIP2                                                                
055700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
055800     MOVE SPACE TO GODK-STATUSKODER                                       
055900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
056000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
056100     PERFORM IMS-STATUSKONTROLL                                           
056200     .                                                                    
056300     SKIP3                                                                
056400 IMS-INSERT-ALT-MSG SECTION.                                              
056500     SKIP2                                                                
056600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
056700     MOVE SPACE TO GODK-STATUSKODER                                       
056800     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
056900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057000     PERFORM IMS-STATUSKONTROLL                                           
057100     .                                                                    
057200     EJECT                                                                
057300 IMS-GET-6001-KNTL SECTION.                                               
057400     SKIP2                                                                
057500     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6001-X ')'                   
057600            DELIMITED BY SIZE INTO SSA1                                   
057700     STRING 'WLJCLA11(IDUSER   =' W-IDUSER ')'                            
057800            DELIMITED BY SIZE INTO SSA2                                   
057900     MOVE '  GE' TO GODK-STATUSKODER                                      
058000     CALL CBLTDLI USING GU JCLA-PCB DLI-IO-AREA SSA1 SSA2                 
058100     MOVE JCLA-STATUS-CODE TO STATUS-WS                                   
058200     PERFORM IMS-STATUSKONTROLL                                           
058300     .                                                                    
058400     EJECT                                                                
058500 IMS-GET-6011-ROT SECTION.                                                
058600     SKIP2                                                                
058700     STRING 'WLJCLC01(WDP101KY =' W-WDP101KY-6011-X ')'                   
058800            DELIMITED BY SIZE INTO SSA1                                   
058900     MOVE '  GE' TO GODK-STATUSKODER                                      
059000     CALL CBLTDLI USING GU JCLC-PCB DLI-IO-AREA SSA1                      
059100     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
059200     PERFORM IMS-STATUSKONTROLL                                           
059300     .                                                                    
059400     SKIP2                                                                
059500 IMS-GET-FIRST-6011-JOB SECTION.                                          
059600     SKIP2                                                                
059700     STRING 'WLJCLC12*F(IDJOB   >=' W-IDJOB-X ')'                         
059800            DELIMITED BY SIZE INTO SSA1                                   
059900     MOVE '  GE' TO GODK-STATUSKODER                                      
060000     CALL CBLTDLI USING GNP JCLC-PCB DLI-IO-AREA SSA1                     
060100     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
060200     PERFORM IMS-STATUSKONTROLL                                           
060300     .                                                                    
060400     SKIP2                                                                
060500 IMS-GET-6011-JOB SECTION.                                                
060600     SKIP2                                                                
060700     STRING 'WLJCLC12(IDJOB   >=' W-IDJOB-X ')'                           
060800            DELIMITED BY SIZE INTO SSA1                                   
060900     MOVE '  GE' TO GODK-STATUSKODER                                      
061000     CALL CBLTDLI USING GNP JCLC-PCB DLI-IO-AREA SSA1                     
061100     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
061200     PERFORM IMS-STATUSKONTROLL                                           
061300     .                                                                    
061400     EJECT                                                                
061500 IMS-GET-HOLD-6011-JOB SECTION.                                           
061600     SKIP2                                                                
061700     STRING 'WLJCLC01*P(WDP101KY =' W-WDP101KY-6011-X ')'                 
061800            DELIMITED BY SIZE INTO SSA1                                   
061900     STRING 'WLJCLC12(IDJOB    =' W-IDJOB-X ')'                           
062000            DELIMITED BY SIZE INTO SSA2                                   
062100     MOVE '  ' TO GODK-STATUSKODER                                        
062200     CALL CBLTDLI USING GHU JCLC-PCB DLI-IO-AREA SSA1 SSA2                
062300     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
062400     PERFORM IMS-STATUSKONTROLL                                           
062500     .                                                                    
062600     SKIP2                                                                
062700 IMS-GET-6011-IDUSER-KNTL SECTION.                                        
062800     SKIP2                                                                
062900     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6011-X ')'                   
063000            DELIMITED BY SIZE INTO SSA1                                   
063100     STRING 'WLJCLA11(IDUSER   =' W-IDUSER-X ')'                          
063200            DELIMITED BY SIZE INTO SSA2                                   
063300     MOVE '  GE' TO GODK-STATUSKODER                                      
063400     CALL CBLTDLI USING GU JCLA-PCB DLI-IO-AREA SSA1 SSA2                 
063500     MOVE JCLA-STATUS-CODE TO STATUS-WS                                   
063600     PERFORM IMS-STATUSKONTROLL                                           
063700     .                                                                    
063800     EJECT                                                                
063900 IMS-GET-HOLD-6021 SECTION.                                               
064000     SKIP2                                                                
064100     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-6021-X ')'                   
064200            DELIMITED BY SIZE INTO SSA1                                   
064300     MOVE '  GE' TO GODK-STATUSKODER                                      
064400     CALL CBLTDLI USING GHU JCLD-PCB DLI-IO-AREA SSA1                     
064500     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
064600     PERFORM IMS-STATUSKONTROLL                                           
064700     .                                                                    
064800     SKIP2                                                                
064900 IMS-GET-6021-IDOWNER-KNTL SECTION.                                       
065000     SKIP2                                                                
065100     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-6021-X ')'                   
065200            DELIMITED BY SIZE INTO SSA1                                   
065300     STRING 'WLJCLD11(IDOWNER  =' W-IDOWNER-X ')'                         
065400            DELIMITED BY SIZE INTO SSA2                                   
065500     MOVE '  GE' TO GODK-STATUSKODER                                      
065600     CALL CBLTDLI USING GU JCLD-PCB DLI-IO-AREA SSA1 SSA2                 
065700     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
065800     MOVE JCLD-SEG-LEVEL TO LEVEL-WS                                      
065900     PERFORM IMS-STATUSKONTROLL                                           
066000     .                                                                    
066100     SKIP2                                                                
066200 IMS-GET-6021-IDUSER-KNTL SECTION.                                        
066300     SKIP2                                                                
066400     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-6021-X ')'                   
066500            DELIMITED BY SIZE INTO SSA1                                   
066600     STRING 'WLJCLD11(IDUSER   =' W-IDUSER-X ')'                          
066700            DELIMITED BY SIZE INTO SSA2                                   
066800     MOVE '  GE' TO GODK-STATUSKODER                                      
066900     CALL CBLTDLI USING GU JCLD-PCB DLI-IO-AREA SSA1 SSA2                 
067000     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
067100     MOVE JCLD-SEG-LEVEL TO LEVEL-WS                                      
067200     PERFORM IMS-STATUSKONTROLL                                           
067300     .                                                                    
067400     EJECT                                                                
067500 IMS-DELETE-6011-JOB SECTION.                                             
067600     SKIP2                                                                
067700     MOVE '  ' TO GODK-STATUSKODER                                        
067800     CALL CBLTDLI USING DLET JCLC-PCB DLI-IO-AREA                         
067900     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
068000     PERFORM IMS-STATUSKONTROLL                                           
068100     .                                                                    
068200     SKIP2                                                                
068300 IMS-DELETE-6021 SECTION.                                                 
068400     SKIP2                                                                
068500     MOVE '  ' TO GODK-STATUSKODER                                        
068600     CALL CBLTDLI USING DLET JCLD-PCB DLI-IO-AREA                         
068700     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
068800     PERFORM IMS-STATUSKONTROLL                                           
068900     .                                                                    
069000     EJECT                                                                
069100 IMS-STATUSKONTROLL SECTION.                                              
069200     SET STATUS-IX TO 1                                                   
069300     SEARCH GODK-STATUS                                                   
069400       AT END                                                             
069500         CALL FELLOG                                                      
069600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
069700         CONTINUE                                                         
069800     END-SEARCH                                                           
069900     .                                                                    
