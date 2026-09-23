000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5135000.                                                
000300 AUTHOR.         JAN PETTERSSON.    PAH.                                  
000400 DATE-WRITTEN.   FEBR  1978.        OKT 90 COBII.                         
000500                                                                          
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION                                                             
000900*                LÄS DEN SORTERADE FILEN OCH                              
001000*                SKAPA DÄREFTER LISTA W51350-001                          
001100*                ENDAST IDLISTA=001 ANVÄNDS                               
001300*    UTDATA                                                               
001400*                LISTOR, NU FIL TILL ONDEMAND..                           
001500*                                                                         
001600*    SUBPROGRAM                                                           
001700*                DATKORT     INLÄSNING AV DATUMKORT                       
001800*                ABEND       AVSLUTAR PGM MED ABEND                       
001900*                WDATKONV                                                 
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200 INPUT-OUTPUT SECTION.                                                    
002300 FILE-CONTROL.                                                            
002400     SELECT  W51351  ASSIGN  UT-S-W51350D1.                               
002500     SELECT  LISTOR  ASSIGN  UT-S-W51350D2.                               
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 FILE SECTION.                                                            
002900                                                                          
003000 FD  W51351                                                               
003100     RECORDING MODE V                                                     
003200     LABEL RECORD STANDARD                                                
003300     BLOCK CONTAINS 0 RECORDS.                                            
003400*01  INPOST     -COPY W5132601 -L.                                        
003500*01  INPOST2    -COPY W5132602 -L.                                        
003600     SKIP3                                                                
003700 FD  LISTOR                                                               
003800     RECORDING F                                                          
003900     LABEL RECORD STANDARD                                                
004000     BLOCK 0                                                              
004100     RECORD 121.                                                          
004200     SKIP2                                                                
004300 01  LISTOR-RAD                PIC X(121).                                
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'W5135000'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  PRIS                        PIC S9(9)V99  VALUE +0  COMP-3.          
005100 77  PRIS2                       PIC 9(9)V99   VALUE 0   COMP-3.          
005200 77  EOF                         PIC X       VALUE 'N'.                   
005300 77  ABEND-CODE                  PIC S9(4)   COMP SYNC VALUE +0.          
005400 77  WS-JUST-VAERDE              PIC S9(9)V99    COMP-3.                  
005500 77  W-SID-RAEKNARE              PIC S9(3)    COMP-3 VALUE ZERO.          
005600 77  W-RAD-RAEKNARE              PIC S9(3)    COMP-3 VALUE +99.           
005700 77  W-RAD-MAXVAERDE             PIC S9(3)    COMP-3 VALUE 37.            
005800                                                                          
006000 01  SPAR-IDDC                   PIC X(2) VALUE SPACE.                    
006900     EJECT                                                                
007100                                                                          
007200 01  FILLER.                                                              
007300     03  INAREA.                                                          
007400*        05  -COPY W5132601 -PRE W2-.                                     
007500     EJECT                                                                
007600     03  FILLER REDEFINES INAREA.                                         
007700*        05  -COPY W5132602 -PRE W1-.                                     
007800     EJECT                                                                
007900 01  SUBPROGRAM.                                                          
008000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008300     SKIP3                                                                
008340*                                                                         
008400 01  PARAMETRAR-TILL-DATKORTRC2.                                          
008500     03  WDATUM                  PIC X(6)    VALUE 'WDATUM'.              
008600*    03  MOTTAG-FLT             -COPY WDATKORT                            
008700     EJECT                                                                
008800*01  -COPY WDATAREA                                                       
013000     SKIP3                                                                
013100 01  LIST1-RUBRIKER.                                                      
013200     03  LIST1-RUBRAD-1.                                                  
013300         05  FILLER              PIC X(11)                                
013400             VALUE    ' W51350-001'.                                      
013500         05  FILLER              PIC X(9)                                 
013600             VALUE SPACE.                                                 
013610         05  FILLER              PIC X(4)                                 
013620             VALUE    'VCCS'.                                             
013630         05  FILLER              PIC X(16)                                
013640             VALUE SPACE.                                                 
013700         05  FILLER              PIC X(33)                                
013800             VALUE 'QUANTITATIVE ADJUSTMENTS  PERIOD'.                    
013900         05  LIST1-TIRP          PIC Z9.                                  
013910         05  FILLER              PIC X(15)                                
013920             VALUE SPACE.                                                 
014000         05  FILLER              PIC X(4)                                 
014100             VALUE 'DC: '.                                                
014200         05  LIST1-IDDC          PIC X(2).                                
014300         05  FILLER              PIC X(4)  VALUE SPACE.                   
014310         05  FILLER              PIC X(6)  VALUE 'DATE: '.                
014400         05  LIST1-D-AAR         PIC 99.                                  
014600         05  LIST1-D-MAANAD      PIC 99.                                  
014800         05  LIST1-D-DAG         PIC 99.                                  
015200     EJECT                                                                
015300     03  LIST1-RUBRAD-2.                                                  
015400         05  FILLER              PIC X(16)                                
015500             VALUE '     PARTNO'.                                         
015600         05  FILLER              PIC X(19)                                
015700             VALUE 'DESCRIPT.'.                                           
015800         05  FILLER              PIC X(13)                                
015900             VALUE 'ADJ. QTY'.                                            
016000         05  FILLER              PIC X(11)                                
016100             VALUE 'ADJ. DATE'.                                           
016200         05  FILLER              PIC X(11)                                
016300             VALUE 'ERSKOD'.                                              
016400         05  FILLER              PIC X(11)                                
016500             VALUE 'LKTO'.                                                
016600         05  FILLER              PIC X(28)                                
016700             VALUE 'ADJ. VALUE'.                                          
016800     SKIP3                                                                
016900 01  LIST1-POST-RAD.                                                      
017000     03  FILLER               PIC X(02)    VALUE SPACE.                   
017100     03  LIST1-IDARTNR        PIC Z(9).                                   
017200     03  FILLER               PIC X(05)    VALUE SPACE.                   
017300     03  LIST1-BEART-ENG      PIC X(15).                                  
017400     03  FILLER               PIC X(05)    VALUE SPACE.                   
017500     03  LIST1-KVJUSTKV       PIC Z(7)-.                                  
017600     03  FILLER               PIC X(04)    VALUE SPACE.                   
017700     03  LIST1-TIM-INV        PIC 99B99B99.                               
017800     03  FILLER               PIC X(07)    VALUE SPACE.                   
017900     03  LIST1-KDERS          PIC 99.                                     
018000     03  FILLER               PIC X(02)    VALUE SPACE.                   
018100     03  LIST1-IDLKTO         PIC Z(6)9.                                  
018200     03  FILLER               PIC X(03)    VALUE SPACE.                   
018300     03  LIST1-JUST-VAERDE    PIC ZZZBZZZBZZ9.99-.                        
018400     03  FILLER               PIC X(27)    VALUE SPACE.                   
018800                                                                          
026000 01  LIST-SPACE-RAD         PIC X(119) VALUE SPACE.                       
026100     EJECT                                                                
026200 PROCEDURE DIVISION.                                                      
026300                                                                          
026600     PERFORM A-INITIERA                                                   
026800                                                                          
026900     PERFORM S01-LAS-IN-POST                                              
027300                                                                          
027400     PERFORM UNTIL EOF = JA                                               
027600       IF W1-IDLISTA = +1                                                 
027700         PERFORM B-BEARBETA-IDLISTA1                                      
028000       END-IF                                                             
028200                                                                          
028300       PERFORM S01-LAS-IN-POST                                            
028400     END-PERFORM                                                          
028500                                                                          
028800     PERFORM Z-AVSLUTA                                                    
028900                                                                          
029000     MOVE +0 TO RETURN-CODE                                               
029100     GOBACK                                                               
029200     .                                                                    
029300     EJECT                                                                
029400 A-INITIERA SECTION.                                                      
029500                                                                          
029600     OPEN INPUT  W51351                                                   
029700          OUTPUT LISTOR                                                   
029800                                                                          
029900     CALL DATKORT USING IDPGM WDATUM MOTTAG-FLT                           
030000     MOVE D-AAR          TO LIST1-D-AAR                                   
030200                            DAT-I-TIDATUM(1:2)                            
030300     MOVE D-MAANAD       TO LIST1-D-MAANAD                                
030500                            DAT-I-TIDATUM(3:2)                            
030600     MOVE D-DAG          TO LIST1-D-DAG                                   
030800                            DAT-I-TIDATUM(5:2)                            
030900                                                                          
031000     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
031100     CALL WDATKONV USING    DAT-KDDATFORM                                 
031200                            DAT-I-TIDATUM                                 
031300                            DAT-O-TIDATUM                                 
031400                            DAT-KDSVAR                                    
031500     IF DAT-KDSVAR-OK                                                     
031600       MOVE DAT-TIRP       TO LIST1-TIRP                                  
031800     ELSE                                                                 
031900       MOVE ZERO           TO LIST1-TIRP                                  
032100     END-IF                                                               
033000     .                                                                    
033100     EJECT                                                                
033200 B-BEARBETA-IDLISTA1 SECTION.                                             
033300                                                                          
033400     MOVE W1-IDDC          TO LIST1-IDDC                                  
033500*    ADD +1                TO W-RAD-RAEKNARE                              
033600                                                                          
033700     IF SPAR-IDDC NOT = W1-IDDC                                           
033900       PERFORM BB-SKRIV-RUBRIK-LISTA1                                     
034000       MOVE W1-IDDC     TO SPAR-IDDC                                      
034100     END-IF                                                               
034200                                                                          
034300     COMPUTE WS-JUST-VAERDE ROUNDED = W1-KVJUSTKV * W1-PRARTSTD           
034400                                                                          
034500     PERFORM BC-SKRIV-RAD-LISTA1                                          
034600     .                                                                    
034700     SKIP2                                                                
034800 BB-SKRIV-RUBRIK-LISTA1   SECTION.                                        
034900                                                                          
035000*    ADD +1              TO W-SID-RAEKNARE                                
035100*    MOVE W-SID-RAEKNARE TO LIST1-PAGE-COUNTER                            
035200     MOVE W1-IDDC        TO LIST1-IDDC                                    
035300                                                                          
035400     WRITE LISTOR-RAD FROM LIST1-RUBRAD-1 AFTER PAGE                      
035500     WRITE LISTOR-RAD FROM LIST1-RUBRAD-2 AFTER 2                         
035600     WRITE LISTOR-RAD FROM LIST-SPACE-RAD AFTER 1                         
035700                                                                          
035800*    MOVE +1             TO W-RAD-RAEKNARE                                
035900     .                                                                    
036000     SKIP2                                                                
036100 BC-SKRIV-RAD-LISTA1   SECTION.                                           
036200                                                                          
036300     MOVE W1-IDARTNR     TO LIST1-IDARTNR                                 
036400     MOVE W1-BEART-ENG   TO LIST1-BEART-ENG                               
036500     MOVE W1-KVJUSTKV    TO LIST1-KVJUSTKV                                
036600     MOVE W1-TIM-INV     TO LIST1-TIM-INV                                 
036700     MOVE W1-KDERS       TO LIST1-KDERS                                   
036800     MOVE W1-IDLKTO      TO LIST1-IDLKTO                                  
036900     MOVE WS-JUST-VAERDE TO LIST1-JUST-VAERDE                             
037000                                                                          
037100     WRITE LISTOR-RAD FROM LIST1-POST-RAD AFTER 1                         
037200     .                                                                    
057800     EJECT                                                                
057900 Z-AVSLUTA SECTION.                                                       
058000                                                                          
058100     CLOSE W51351 LISTOR                                                  
058200     SKIP3                                                                
058300     .                                                                    
058400 S01-LAS-IN-POST SECTION.                                                 
058500                                                                          
058600     READ W51351 INTO INAREA                                              
058700     AT END                                                               
058800         MOVE JA TO EOF                                                   
058900     END-READ                                                             
059000     .                                                                    
