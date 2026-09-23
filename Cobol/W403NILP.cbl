000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W403NILP.                                                
000400 AUTHOR.         GÖRAN KJELLSON    GUIDE                                  
000500 DATE-WRITTEN.   DECEMBER 2006.                                           
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET SKAPAR MAIL VIA D&P                                   
001000*        MED NOLLPLOCK ('NIL PICK') VID PACKNINGSRAPPORTERING             
001010*                                                                         
001020*    ABENDCODES:                                                          
001030*        U1000 - D&P ERROR                                                
001100*                                                                         
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 DATA DIVISION.                                                           
001800 WORKING-STORAGE SECTION.                                                 
001810                                                                          
001900 77  IDPGM                       PIC X(8)    VALUE 'W403NILP'.            
001910 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
001920 77  CURRENT-DAP-SECTION         PIC X(16)   VALUE SPACE.                 
002000 77  JA                          PIC X       VALUE 'J'.                   
002100 77  NEJ                         PIC X       VALUE 'N'.                   
002110 77  KDRC-DISPLAY                PIC Z(5).                                
002200                                                                          
002250 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
002260 01  FILLER REDEFINES DAGENS-DATUM.                                       
002270     03  DAGENS-DATUM-AAR        PIC 9(2).                                
002280     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
002290     03  DAGENS-DATUM-DAG        PIC 9(2).                                
002600                                                                          
002700 01  DYNAMISKA-SUBPROGRAM.                                                
002800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
002900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
002910     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
002920                                                                          
002921 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
002922                                                                          
002930 01  ERRTEXT.                                                             
002940     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
002950     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
003000                                                                          
003100                                                                          
003200*    --- COMMUNICATION AREAS                                              
003300*                                                                         
003400 01  HDR-AREA.                                                            
003500*    03  -COPY WZ01REQU                                                   
003600*    03  -COPY WZ04HDR                                                    
003700                                                                          
003800 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
003900 01  SEND-AREA.                                                           
004000*    03  -COPY WZ01SEND                                                   
004100                                                                          
004200 01  SEND-RAD                    PIC X(120)  VALUE SPACE.                 
004300                                                                          
004400 01  FILLER                      PIC X(16)   VALUE 'OILIST'.              
004500                                                                          
004600*    --- LISTLAYOUT                                                       
004700 01  NP-LIST.                                                             
004800     03  NP-RUBRIK1.                                                      
004900         05  FILLER              PIC X(25)   VALUE                        
005000            'VOLVO CAR CORP., NIL PICK'.                                  
005200         05  FILLER              PIC X(2)    VALUE SPACE.                 
005300         05  NPR1-DATUM          PIC 9(6).                                
008100                                                                          
008200     03  NP-DISTRIKT.                                                     
008210         05  FILLER              PIC X(9)    VALUE 'DISTRICT '.           
008400         05  FILLER              PIC X(1)    VALUE SPACE.                 
008410         05  NP-RAD-IDDISTR      PIC 9(4).                                
008420                                                                          
008430     03  NP-KUND.                                                         
008440         05  FILLER              PIC X(9)    VALUE 'CUSTOMER '.           
008450         05  FILLER              PIC X(1)    VALUE SPACE.                 
008460         05  NP-RAD-IDKUNDNR     PIC 9(6).                                
008470                                                                          
008480     03  NP-ORDER.                                                        
008490         05  FILLER              PIC X(9)    VALUE 'ORDER NO '.           
008500         05  FILLER              PIC X(1)    VALUE SPACE.                 
008600         05  NP-RAD-IDKUNDRF     PIC X(10).                               
008700                                                                          
008800     03  NP-DC.                                                           
008900         05  FILLER              PIC X(9)    VALUE 'DC       '.           
009000         05  FILLER              PIC X(1)    VALUE SPACE.                 
009100         05  NP-RAD-IDDC         PIC X(2).                                
009200                                                                          
009300     03  NP-PLOCKARE.                                                     
009400         05  FILLER              PIC X(9)    VALUE 'PICKER   '.           
009500         05  FILLER              PIC X(1)    VALUE SPACE.                 
009600         05  NP-RAD-IDANSTNR     PIC 9(5).                                
009700                                                                          
009800     03  NP-ARTIKEL.                                                      
009900         05  FILLER              PIC X(9)    VALUE 'PART     '.           
009910         05  FILLER              PIC X(1)    VALUE SPACE.                 
009920         05  NP-RAD-IDARTNR      PIC 9(8).                                
009930                                                                          
009940     03  NP-ANTALRUBRIK          PIC X(9)    VALUE 'QUANTITY '.           
009941     03  NP-BESTANT.                                                      
009950         05  FILLER              PIC X(9)    VALUE 'ORDERED  '.           
009960         05  FILLER              PIC X(1)    VALUE SPACE.                 
009970         05  NP-RAD-KVBEART      PIC 9(6).                                
009980                                                                          
009990     03  NP-ANNANT.                                                       
009991         05  FILLER              PIC X(9)    VALUE 'CANCELLED'.           
009992         05  FILLER              PIC X(1)    VALUE SPACE.                 
009993         05  NP-RAD-KVANNANT     PIC 9(6).                                
009994                                                                          
009995     03  NP-AVBOKAT.                                                      
009996         05  FILLER              PIC X(9)    VALUE 'ALLOCATED'.           
009997         05  FILLER              PIC X(1)    VALUE SPACE.                 
009998         05  NP-RAD-KVAVBART     PIC 9(6).                                
009999                                                                          
010000     03  NP-LEVERERAT.                                                    
010010         05  FILLER              PIC X(9)    VALUE 'DELIVERED'.           
010020         05  FILLER              PIC X(1)    VALUE SPACE.                 
010030         05  NP-RAD-KVLEVART     PIC 9(6).                                
010100                                                                          
018000                                                                          
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400*01  -COPY W403NILP                                                       
018600*01  -COPY W0009            -PRE DISTRDOC-                                
018700                                                                          
018800                                                                          
018900 PROCEDURE DIVISION  USING  NILP-W403NILP DISTRDOC-PCB.                   
019000 STYR SECTION.                                                            
019100                                                                          
019200     PERFORM A-INIT                                                       
019210     PERFORM S90-SEND-OPEN                                                
019220     PERFORM S90-PUT-DAP-START                                            
019300                                                                          
019400     PERFORM B-REDIGERA-RUBRIK                                            
019410     PERFORM C-REDIGERA-RADER                                             
019500                                                                          
019510     PERFORM S90-SEND-CLOSE                                               
019600     GOBACK                                                               
019700     .                                                                    
019800                                                                          
020000 A-INIT        SECTION.                                                   
020100     MOVE 'A-INIT'  TO CURRENT-SECTION.                                   
020200                                                                          
020500     ACCEPT DAGENS-DATUM  FROM DATE                                       
020800     .                                                                    
020900                                                                          
021000                                                                          
021100 B-REDIGERA-RUBRIK SECTION.                                               
021200     MOVE 'B-REDIGERA-RUBRIK' TO CURRENT-SECTION.                         
021300                                                                          
021500     MOVE DAGENS-DATUM     TO NPR1-DATUM                                  
021600     MOVE NP-RUBRIK1       TO SEND-RAD                                    
021700     PERFORM S90-PUT-DOC-LINE                                             
021800                                                                          
021900     MOVE SPACE            TO SEND-RAD                                    
022000     PERFORM S90-PUT-DOC-LINE                                             
024000                                                                          
024100     MOVE SPACE            TO SEND-RAD                                    
024200     PERFORM S90-PUT-DOC-LINE                                             
024300     .                                                                    
024400                                                                          
024500                                                                          
024600 C-REDIGERA-RADER  SECTION.                                               
024700     MOVE 'C-REDIGERA-RADER' TO CURRENT-SECTION.                          
024800                                                                          
024900     MOVE NILP-IDDISTR             TO NP-RAD-IDDISTR                      
025800     MOVE NP-DISTRIKT              TO SEND-RAD                            
025900     PERFORM S90-PUT-DOC-LINE                                             
025910                                                                          
025920     MOVE NILP-IDKUNDNR            TO NP-RAD-IDKUNDNR                     
025930     MOVE NP-KUND                  TO SEND-RAD                            
025940     PERFORM S90-PUT-DOC-LINE                                             
025950                                                                          
025960     MOVE NILP-IDKUNDRF            TO NP-RAD-IDKUNDRF                     
025970     MOVE NP-ORDER                 TO SEND-RAD                            
025980     PERFORM S90-PUT-DOC-LINE                                             
025990                                                                          
025991     MOVE NILP-IDDC                TO NP-RAD-IDDC                         
025992     MOVE NP-DC                    TO SEND-RAD                            
025993     PERFORM S90-PUT-DOC-LINE                                             
025994                                                                          
025995     MOVE NILP-IDANSTNR            TO NP-RAD-IDANSTNR                     
025996     MOVE NP-PLOCKARE              TO SEND-RAD                            
025997     PERFORM S90-PUT-DOC-LINE                                             
025998                                                                          
025999     MOVE NILP-IDARTNR             TO NP-RAD-IDARTNR                      
026000     MOVE NP-ARTIKEL               TO SEND-RAD                            
026001     PERFORM S90-PUT-DOC-LINE                                             
026002                                                                          
026004     MOVE NP-ANTALRUBRIK           TO SEND-RAD                            
026005     PERFORM S90-PUT-DOC-LINE                                             
026006                                                                          
026007     MOVE NILP-KVBEART             TO NP-RAD-KVBEART                      
026008     MOVE NP-BESTANT               TO SEND-RAD                            
026009     PERFORM S90-PUT-DOC-LINE                                             
026010                                                                          
026011     MOVE NILP-KVANNANT            TO NP-RAD-KVANNANT                     
026012     MOVE NP-ANNANT                TO SEND-RAD                            
026013     PERFORM S90-PUT-DOC-LINE                                             
026014                                                                          
026015     MOVE NILP-KVAVBART            TO NP-RAD-KVAVBART                     
026016     MOVE NP-AVBOKAT               TO SEND-RAD                            
026017     PERFORM S90-PUT-DOC-LINE                                             
026018                                                                          
026019     MOVE NILP-KVLEVART            TO NP-RAD-KVLEVART                     
026020     MOVE NP-LEVERERAT             TO SEND-RAD                            
026021     PERFORM S90-PUT-DOC-LINE                                             
026030     .                                                                    
026100                                                                          
026200                                                                          
029100*    --- DISPATCHER SECTIONS                                              
029200 S90-SEND-OPEN SECTION.                                                   
029300     MOVE 'S90-SEND-OPEN'           TO CURRENT-SECTION.                   
029400                                                                          
029500     MOVE 'OPEN'                    TO SEND-KDFUNC                        
029600     MOVE 'CARPARTS.DAP.DISTRDOC'   TO SEND-ADDISPABS                     
029700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
029800                         SEND-OPEN-AREA                                   
029900     IF SEND-KDRC > 0                                                     
030000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
030100       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
030200       DELIMITED BY SIZE INTO ERRTEXT                                     
030300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030400     END-IF                                                               
030500     .                                                                    
030600                                                                          
030700                                                                          
030800 S90-PUT-DAP-START SECTION.                                               
030900     MOVE 'S90-PUT-DAP-START'     TO CURRENT-SECTION.                     
031000                                                                          
031100     MOVE 1                       TO REQU-IDMSGVER                        
031200     MOVE 'R'                     TO REQU-KDPGMACT                        
031300     MOVE IDPGM                   TO REQU-IDUSER                          
031500     MOVE 'NIL PICK'              TO HDR-IDOUTTYPE                        
032300     MOVE SPACE                   TO HDR-IDOUTREC                         
032400                                     HDR-IDLIST                           
032500     MOVE 'NP'                    TO HDR-IDOUTREC (1:2)                   
032600                                     HDR-IDLIST                           
032700     MOVE 'PUT'                   TO SEND-KDFUNC                          
032800     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
032900                                                                          
033000                                                                          
033100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
033200                         SEND-KVDLEN                                      
033300                         HDR-AREA                                         
033400     IF SEND-KDRC > ZERO                                                  
033500       MOVE SEND-KDRC               TO KDRC-DISPLAY                       
033600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
033700       DELIMITED BY SIZE          INTO ERRTEXT-STR                        
033800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033900     END-IF                                                               
034000     .                                                                    
034100                                                                          
034200                                                                          
034300 S90-PUT-DOC-LINE SECTION.                                                
034400     MOVE 'S90-PUT-DOC-LINE' TO CURRENT-SECTION.                          
034500                                                                          
034600     MOVE 'PUT'                           TO SEND-KDFUNC                  
034700     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
034800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
034900                         SEND-KVDLEN                                      
035000                         SEND-RAD                                         
035100     IF SEND-KDRC > ZERO                                                  
035200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
035300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
035400       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
035500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035600     END-IF                                                               
035700     .                                                                    
035800                                                                          
035900                                                                          
036000 S90-SEND-CLOSE SECTION.                                                  
036100     MOVE 'S90-SEND-CLOSE' TO CURRENT-SECTION.                            
036200                                                                          
036300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
036400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036500                                                                          
036600     IF SEND-KDRC > 0                                                     
036700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
036800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
036900       DELIMITED BY SIZE INTO ERRTEXT                                     
037000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037100     END-IF                                                               
037200     .                                                                    
