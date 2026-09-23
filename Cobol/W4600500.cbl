000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W4600500.                                                 
000300 AUTHOR.        INGEMAR BENGTSON.                                         
000400 DATE-WRITTEN.  NOV 1984.                                                 
000500     REMARKS.                                                             
000600*                                                                         
000700*    FUNKTION:                                                            
000800*        PROGRAMMET KÖRS VID VARJE RDE-ÖVERFÖRING FRÅN IMPORTÖR.          
000900*                                                                         
001000*        PROGRAMMET LÄSER IN EN FIL W46001 SOM INNEHÅLLER ORDER           
001100*        FRÅN EN RDE-ÖVERFÖRING. FILEN BESTÅR AV EN ELLER FLERA           
001200*        SEKVENSER AV STARTKORT, ORDER (EN ELLER FLERA),                  
001300*        EV ÄVEN KREDITERNINGSTRANSAR (RHD),                              
001400*        BYTESTRANSAR (RHM),ORDERBEKRÄFTELSETRANSAR (RHN),                
001500*        SLUTKORT. EN ORDER BESTÅR AV ORDERHUVUD, ETT ANTAL               
001600*        ORDERRADER OCH HASH-TOTAL.                                       
001700*        ALLA ORDER-POSTER SKRIVS PÅ MASTERFILEN W46008.                  
001800*        RÄTTA ORDERNA SKRIVS PÅ W46006.                                  
001900*        FELAKTIGA ORDERNA SKRIVS PÅ W46009.                              
002000*                                                                         
002100*     ****************  KOMPLETTERA VID NOAC INST. **********             
002200*     *******************************************************             
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 OM FEL VID SORTERING                                       
002600*                                                                         
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 CONFIGURATION SECTION.                                                   
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300*                                                                         
003400 FILE-CONTROL.                                                            
003500     SKIP2                                                                
003600*--- INFILER:                                                             
003700*                                                                         
003800     SELECT W46001                       ASSIGN TO UT-S-W46005D1.         
003900     SKIP2                                                                
004000*--- UTFILER:                                                             
004100*                                                                         
004200     SELECT W46006                       ASSIGN TO UT-S-W46005D3.         
004300     SELECT W46008                       ASSIGN TO UT-S-W46005D5.         
004400     SELECT W46009                       ASSIGN TO UT-S-W46005D6.         
004500     SELECT W46029                       ASSIGN TO UT-S-W46005D7.         
004600     SELECT W4601S                       ASSIGN TO UT-S-W46005D8.         
004700     SELECT W46010                       ASSIGN TO UT-S-W46005D9.         
004800     SELECT W46011                       ASSIGN TO UT-S-W46005DA.         
004900     SELECT W46012                       ASSIGN TO UT-S-W46005DB.         
005000     SELECT SORTFIL                      ASSIGN TO UT-S-W46005DS.         
005100     SKIP2                                                                
005200     EJECT                                                                
005300 DATA DIVISION.                                                           
005400     SKIP2                                                                
005500 FILE SECTION.                                                            
005600     SKIP3                                                                
005700 FD  W46001                                                               
005800     LABEL RECORD   STANDARD                                              
005900     RECORDING      V                                                     
006000     BLOCK CONTAINS 0.                                                    
006100     SKIP2                                                                
006200*    -COPY W460RH0      -L.                                               
006300     SKIP2                                                                
006400*    -COPY W460RHA      -L.                                               
006500     SKIP2                                                                
006600 01   FILLER.                                                             
006700*    03 -COPY WZ01REQU     -L.                                            
007800*    03 -COPY W460RHA      -L.                                            
007900     SKIP2                                                                
008000 01   FILLER.                                                             
008100*    03 -COPY WZ01REQU     -L.                                            
008200*    03 -COPY W460RHB      -L.                                            
008300     SKIP2                                                                
008400*    -COPY W460RHB      -L.                                               
008500     SKIP2                                                                
008600*    -COPY W460RHC      -L.                                               
008700     SKIP2                                                                
008800*    -COPY W460RHD      -L.                                               
008900     SKIP2                                                                
009000*    -COPY W460RHDM     -L.                                               
009100     SKIP2                                                                
009200*    -COPY W460RHE      -L.                                               
009300     SKIP2                                                                
009400*    -COPY W460RHF      -L.                                               
009500     SKIP2                                                                
009600*    -COPY W460RH9      -L.                                               
009700     SKIP2                                                                
009800     EJECT                                                                
009810*FILLER IN SORT POST MUST BE CHANGED IF YOU CHANGE COPYTXT W460001        
009900 FD  W46006                                                               
010000     LABEL RECORD   STANDARD                                              
010100     RECORDING      V                                                     
010200     BLOCK CONTAINS 0.                                                    
010300     SKIP2                                                                
010400*01  W460001   -COPY W460001  -PRE W46006-  -L.                           
010500     SKIP2                                                                
010600*01  W460002   -COPY W460002  -PRE W46006-  -L.                           
010700     SKIP2                                                                
010800*01  W460003   -COPY W460003  -PRE W46006-  -L.                           
010900     EJECT                                                                
011000 FD  W46008                                                               
011100     LABEL RECORD   STANDARD                                              
011200     RECORDING      V                                                     
011300     BLOCK CONTAINS 0.                                                    
011400     SKIP2                                                                
011500*01  W460001   -COPY W460001  -PRE W46008-  -L.                           
011600     SKIP2                                                                
011700*01  W460002   -COPY W460002  -PRE W46008-  -L.                           
011800     SKIP2                                                                
011900*01  W460003   -COPY W460003  -PRE W46008-  -L.                           
012000     EJECT                                                                
012100 FD  W46009                                                               
012200     LABEL RECORD   STANDARD                                              
012300     RECORDING      V                                                     
012400     BLOCK CONTAINS 0.                                                    
012500     SKIP2                                                                
012600*01  W460001   -COPY W460001  -PRE W46009-  -L.                           
012700     SKIP2                                                                
012800*01  W460002   -COPY W460002  -PRE W46009-  -L.                           
012900     SKIP2                                                                
013000*01  W460003   -COPY W460003  -PRE W46009-  -L.                           
013100     EJECT                                                                
013200 FD  W46029                                                               
013300     LABEL RECORD   STANDARD                                              
013400     RECORDING      V                                                     
013500     BLOCK CONTAINS 0.                                                    
013600     SKIP2                                                                
013700*01  W418R14   -COPY W418R14  -PRE W46029-  -L.                           
013800     SKIP2                                                                
013900*01  W418R15   -COPY W418R15  -PRE W46029-  -L.                           
014000     SKIP2                                                                
014100*01  W418RHF   -COPY W418RHF  -PRE W46029-  -L.                           
014200     SKIP2                                                                
014300 FD  W4601S                                                               
014400     LABEL RECORD   STANDARD                                              
014500     RECORDING      V                                                     
014600     BLOCK CONTAINS 0.                                                    
014700     SKIP2                                                                
014800*01  W460006   -COPY W460006  -PRE W4601S-  -L.                           
014900     EJECT                                                                
015000     SKIP2                                                                
015100 FD  W46010                                                               
015200     LABEL RECORD   STANDARD                                              
015300     RECORDING      V                                                     
015400     BLOCK CONTAINS 0.                                                    
015500     SKIP2                                                                
015600 01  W46010-KUND-POST    PIC X(80).                                       
015700     EJECT                                                                
015800 FD  W46011                                                               
015900     LABEL RECORD   STANDARD                                              
016000     RECORDING F                                                          
016100     BLOCK CONTAINS 0.                                                    
016200     SKIP2                                                                
016300 01  W46011-BYTES-POST   PIC X(62).                                       
016400     EJECT                                                                
016500 FD  W46012                                                               
016600     LABEL RECORD   STANDARD                                              
016700     RECORDING F                                                          
016800     BLOCK CONTAINS 0.                                                    
016900     SKIP2                                                                
017000 01  W46012-OBKR-POST    PIC X(80).                                       
017100     EJECT                                                                
017200 SD  SORTFIL                                                              
017300     RECORDING V.                                                         
017400     SKIP2                                                                
017500 01  SORT-POST.                                                           
017600     SKIP2                                                                
017700     03  SORT-IDDISTR-1          PIC 9(4).                                
017800     03  SORT-TIFILDAT           PIC 9(6).                                
017900     03  SORT-TIHHMMSS           PIC 9(6).                                
018000     03  SORT-IDPTYP             PIC X(3).                                
018100     03  SORT-IDDISTR-2          PIC 9(4).                                
018200     03  SORT-IDKUNDNR           PIC 9(6).                                
018300     03  SORT-IDORDNR            PIC 9(7).                                
018400     03  FILLER                  PIC X(237).                              
018500     03  SORT-START-SLUT-NR      PIC 9(3).                                
018600     03  SORT-IDTRANSLOP         PIC 9(5).                                
018610*                                                                         
018620*FILLER MUST BE CHANGED IF YOU CHANGE THE COPYTEXT W460001.               
018700*                                                                         
018800     EJECT                                                                
018900 WORKING-STORAGE SECTION.                                                 
019000*    -- CHECKED BY WY2000                                                 
019100     SKIP3                                                                
019200*                                                                         
019300 77   PROGRAM-NAMN           VALUE 'W4600500'                             
019400                                 PIC X(8).                                
019500*                                      GENERERAT PROGRAM-NAMN.            
019600     SKIP2                                                                
019700 01  GENERELLA-KONSTANTER.                                                
019800*                                                                         
019900     03  JA                      PIC X(1)    VALUE 'J'.                   
020000     03  NEJ                     PIC X(1)    VALUE 'N'.                   
020100     SKIP3                                                                
020200 01  SPAR-AREA.                                                           
020300     03 SPAR-OHUV-SORT-IDDISTR         PIC 9(4)  VALUE ZERO.              
020400     03 SPAR-OHUV-SORT-TIFILDAT        PIC 9(6)  VALUE ZERO.              
020500     03 SPAR-OHUV-SORT-TIHHMMSS        PIC 9(6)  VALUE ZERO.              
020600     03 SPAR-OHUV-IDDISTR              PIC 9(4)  VALUE ZERO.              
020700     03 SPAR-OHUV-IDKUNDNR             PIC 9(6)  VALUE ZERO.              
020800     03 SPAR-OHUV-IDORDNR              PIC 9(7)  VALUE ZERO.              
020900     03 SPAR-OHUV-IDPTYP               PIC X(3)  VALUE SPACE.             
021009     03 SPAR-REQU-IDMSGVER             PIC X(3)  VALUE SPACE.             
021110     03 SPAR-REQU-IDUSER-GRP.                                             
021310       05 SPAR-REQU-KDPGMACT           PIC X(1)  VALUE SPACE.             
021410       05 SPAR-REQU-IDUSER             PIC X(8)  VALUE SPACE.             
021500     SKIP3                                                                
021600 01  HJALPAREOR.                                                          
021700     03  JMF-IDDISTR                   PIC 9(4)  VALUE ZERO.              
021800     03  JMF-IDKUNDNR                  PIC 9(6)  VALUE ZERO.              
021900     03  JMF-IDORDNR                   PIC 9(7)  VALUE ZERO.              
022000     03  JMF-IDPTYP                    PIC X(3)  VALUE SPACE.             
022100     03  WS-ANT-TRANS                  PIC S9(5) VALUE ZERO.              
022200     03  WS-IDTRANSLOP                 PIC S9(5) VALUE ZERO.              
022300     03  WS-START-SLUT-NR              PIC 9(3)  VALUE ZERO.              
022400     03  WS-SLUTKORT                   PIC X(1)  VALUE 'N'.               
022500     03  WS-TIFILDAT                   PIC 9(6)  VALUE ZERO.              
022600     03  WS-TIHHMMSS                   PIC 9(6)  VALUE ZERO.              
022700     03  W-TIM-LEVANM          PIC S9(7) COMP-3  VALUE ZERO.              
022800     03  WS-HASH-SUM           PIC S9(15) COMP-3 VALUE ZERO.              
022900     03  WS-BYTES-IDDISTR              PIC 9(4)  VALUE ZERO.              
023000     03  ORDER-FINNS-REDAN-SW          PIC X     VALUE 'J'.               
023100         88  ORDER-FINNS-REDAN                   VALUE 'J'.               
023200     SKIP3                                                                
023300 01  DYNAMISKA-SUBPROGRAM.                                                
023400*                                                                         
023500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
023600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
023700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
023800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
023900     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
024000     EJECT                                                                
024100 01  RETURKODER.                                                          
024200*                                                                         
024300     03  RKOD-ABEND-UTAN-DUMP1   PIC S9(4)   COMP SYNC VALUE +16.         
024400     03  RKOD-ABEND-UTAN-DUMP2   PIC S9(4)   COMP SYNC VALUE +17.         
024500     SKIP3                                                                
024600 01  END-OF-FILE-SWITCHAR.                                                
024700*                                                                         
024800     03  W46001-EOF              PIC X(1)    VALUE 'N'.                   
024900     03  SORTFIL-EOF             PIC X(1)    VALUE 'N'.                   
025000     SKIP2                                                                
025100*                                                                         
025200 01  TEST-IDPTYP                 PIC X(3).                                
025300 01  FILLER  REDEFINES TEST-IDPTYP.                                       
025400     03  IDPTYP-TEST             PIC X(3).                                
025500         88  GILTLIG-IDPTYP      VALUE '.HD' '.TR'                        
025600                                       '.UH' '.UT'                        
025700                                       'RH0' 'RHA'                        
025800                                       'RHB' 'RHC'                        
025900                                       'RHD' 'RHE'                        
026000                                       'RHF' 'RHG'                        
026100                                       'RHH' 'RHM'                        
026200                                       'RHN' 'RH9'.                       
026300     SKIP2                                                                
026400 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
026500*01  FILLER -COPY WWDIS130 -RED TEST-IDDISTR.                             
026600     EJECT                                                                
026700*01  FILLER -COPY WWDIST61 -RED TEST-IDDISTR.                             
026800     EJECT                                                                
026900*01  FILLER -COPY WWDIS100 -RED TEST-IDDISTR.                             
027000     EJECT                                                                
027100*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
027200*                                                                         
027300*01  -COPY W0005      -PRE  POSTSUM-                                      
027400     EJECT                                                                
027500*- - - - - - - - - - - - - - - - PARAMETRAR TILL W460DIS1                 
027600*                                                                         
027700*01  -COPY W460DIS1                                                       
027800     EJECT                                                                
027900*01  -COPY W460LISO                                                       
028000     EJECT                                                                
028100 01  I01-AREA-START               PIC X(8) VALUE                          
028200                                  'I01-AREA'.                             
028300     SKIP3                                                                
028400 01  I01-AREA.                                                            
028500     SKIP2                                                                
028600     03  I01-IDPTYP               PIC X(3).                               
028700     03  FILLER                   PIC X(400).                             
028800     EJECT                                                                
028900*01  FILLER  -PRE I01-  -COPY W460RH0 -RED I01-AREA                       
029000     EJECT                                                                
029100*01  FILLER  -PRE I01-  -COPY W460RHA -RED I01-AREA                       
029200     EJECT                                                                
029300*01  FILLER  -PRE I01-  -COPY W460RHB -RED I01-AREA                       
029400     EJECT                                                                
029500*01  FILLER  -PRE I01-  -COPY W460RHC -RED I01-AREA                       
029600     EJECT                                                                
029700*01  FILLER  -PRE I01-  -COPY W460RHD  -RED I01-AREA                      
029800     EJECT                                                                
029900*01  FILLER  -PRE I01-R15-  -COPY W460RHE -RED I01-AREA                   
030000     EJECT                                                                
030100*01  FILLER  -PRE I01-RHF-  -COPY W460RHF -RED I01-AREA                   
030200     EJECT                                                                
030300*01  FILLER -COPY W460RHG -RED I01-AREA                                   
030400     EJECT                                                                
030500*01  FILLER -COPY W460RHH -RED I01-AREA                                   
030600     EJECT                                                                
030700*01  FILLER -COPY W460RHM -RED I01-AREA                                   
030800     EJECT                                                                
030900*01  FILLER -COPY W460RHN -RED I01-AREA                                   
031000     EJECT                                                                
031100*01  FILLER  -PRE I01-  -COPY W460RH9   -RED I01-AREA                     
031200     EJECT                                                                
031300                                                                          
031400 01  FILLER             PIC X(16) VALUE 'I01-AREA2'.                      
031500 01  I01-AREA2.                                                           
031600     SKIP2                                                                
031700*    03  -PRE I01-   -COPY WZ01REQU                                       
031800     03  I01-AREAREST             PIC X(400).                             
031900                                                                          
032000     EJECT                                                                
032100 01  FILLER             PIC X(9) VALUE 'W371-AREA'.                       
032200     SKIP3                                                                
032300*01  FILLER -COPY W37116 -PRE W371-                                       
032400     EJECT                                                                
032500 01  U06-AREA-START               PIC X(8) VALUE                          
032600                                  'U06-AREA'.                             
032700     SKIP3                                                                
032800 01  U06-AREA.                                                            
032900     SKIP2                                                                
033000*03  FILLER  -PRE U06-  -COPY W460001                                     
033100     EJECT                                                                
033200*01  FILLER  -PRE U06-  -COPY W460002 -RED U06-AREA                       
033300     EJECT                                                                
033400 01  U08-AREA-START               PIC X(8) VALUE                          
033500                                  'U08-AREA'.                             
033600     SKIP3                                                                
033700 01  U08-AREA.                                                            
033800     SKIP2                                                                
033900*03  FILLER  -PRE U08-  -COPY W460001                                     
034000     EJECT                                                                
034100*01  FILLER  -PRE U08-  -COPY W460002 -RED U08-AREA                       
034200     EJECT                                                                
034300*01  FILLER  -PRE U08-  -COPY W460003 -RED U08-AREA                       
034400     EJECT                                                                
034500 01  U09-AREA-START               PIC X(8) VALUE                          
034600                                  'U09-AREA'.                             
034700     SKIP3                                                                
034800 01  U09-AREA.                                                            
034900     SKIP2                                                                
035000*03  FILLER  -PRE U09-  -COPY W460001                                     
035100     EJECT                                                                
035200*01  FILLER  -PRE U09-  -COPY W460002 -RED U09-AREA                       
035300     EJECT                                                                
035400*01  FILLER  -PRE U09-  -COPY W460003 -RED U09-AREA                       
035500     EJECT                                                                
035600 01  U29-AREA-START               PIC X(8) VALUE                          
035700                                  'U29-AREA'.                             
035800     SKIP2                                                                
035900 01  U29-AREA.                                                            
036000     SKIP2                                                                
036100*03  R14     -PRE U29-R14-  -COPY W418R14                                 
036200     SKIP2                                                                
036300*03  R15     -PRE U29-R15-  -COPY W418R15   -RED U29-R14-R14.             
036400     SKIP2                                                                
036500*03  RHF     -PRE U29-RHF-  -COPY W418RHF   -RED U29-R14-R14.             
036600     EJECT                                                                
036700 01  U1S-AREA-START               PIC X(8) VALUE                          
036800                                  'U1S-AREA'.                             
036900                                                                          
037000 01  U1S-AREA.                                                            
037100     SKIP2                                                                
037200*03  FILLER  -PRE U1S-  -COPY W460006                                     
037300     EJECT                                                                
037400 01  SORTWS-AREA-START            PIC X(24) VALUE                         
037500                                  'SORTWS-AREA'.                          
037600     SKIP3                                                                
037700 01  SORTWS-AREA.                                                         
037800     SKIP2                                                                
037900     EJECT                                                                
038000*03  FILLER  -PRE SORTWS-  -COPY W460001                                  
038100     06  SORTWS-START-SLUT-NR            PIC 9(3).                        
038200     06  SORTWS-IDTRANSLOP               PIC 9(5).                        
038300     EJECT                                                                
038400*01  FILLER  -PRE SORTWS-  -COPY W460002  -RED SORTWS-AREA                
038500     EJECT                                                                
038600*01  FILLER  -PRE SORTWS-  -COPY W460003  -RED SORTWS-AREA                
038700     EJECT                                                                
038800 01  FELTABELL.                                                           
038900     SKIP1                                                                
039000     03  FELTYP OCCURS 100 INDEXED BY IX-SEKVENS.                         
039100         05  WS-KDFEL                  PIC 9(3).                          
039200 01  IMS-AREA-START              PIC X(24)   VALUE                        
039300                                            'IMS-AREA-START'.             
039400     EJECT                                                                
039500*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
039600*                                                                         
039700 01  IMS-WS.                                                              
039800   03  FILLER                    PIC X(8)    VALUE 'IMS-WS  '.            
039900     SKIP3                                                                
040000*                            *** STATUSKOD FRÅN IMS                       
040100   03  STATUS-WS                 PIC XX.                                  
040200     88  SEGMENT-FINNS                       VALUE '  '.                  
040300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
040400     SKIP3                                                                
040500   03  GODK-STATUSKODER.                                                  
040600     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040700     SKIP3                                                                
040800   03  SSA1                      PIC X(96).                               
040900     EJECT                                                                
041000*01  -COPY W0003                                                          
041100     EJECT                                                                
041200 LINKAGE SECTION.                                                         
041300*01   -COPY W0008   -PRE ORQL-                                            
041400      05 FILLER     PIC X.                                                
041500     EJECT                                                                
041600 PROCEDURE DIVISION USING ORQL-PCB.                                       
041700     ENTRY 'DLITCBL' USING ORQL-PCB.                                      
041800     SKIP2                                                                
041900                                                                          
042000     PERFORM A-INIT                                                       
042100     SKIP2                                                                
042200     SORT SORTFIL    ASCENDING KEY SORT-IDDISTR-1                         
042300                                   SORT-TIFILDAT                          
042400                                   SORT-TIHHMMSS                          
042500                                   SORT-IDDISTR-2                         
042600                                   SORT-IDKUNDNR                          
042700                                   SORT-IDORDNR                           
042800                                   SORT-IDPTYP                            
042900                                   SORT-IDTRANSLOP                        
043000          INPUT PROCEDURE C-KONTROLL                                      
043100          OUTPUT PROCEDURE D-BEARBETA                                     
043200     IF SORT-RETURN > ZERO                                                
043300         DISPLAY '*** W46005 FEL VID SORTERING ***'                       
043400         CALL ABEND USING RKOD-ABEND-UTAN-DUMP1                           
043500     END-IF                                                               
043600     SKIP2                                                                
043700     PERFORM Z-FINIT                                                      
043800     SKIP2                                                                
043900     MOVE ZERO TO RETURN-CODE                                             
044000     GOBACK                                                               
044100     .                                                                    
044200     EJECT                                                                
044300 A-INIT SECTION.                                                          
044400     SKIP2                                                                
044500     OPEN INPUT W46001                                                    
044600     SKIP2                                                                
044700     OPEN OUTPUT W46006                                                   
044800                 W46008                                                   
044900                 W46009                                                   
045000                 W46029                                                   
045100                 W4601S                                                   
045200                 W46010                                                   
045300                 W46011                                                   
045400                 W46012                                                   
045500     SKIP2                                                                
045600     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
045700     .                                                                    
045800     EJECT                                                                
045900 C-KONTROLL SECTION.                                                      
046000     SKIP2                                                                
046100     MOVE 0 TO WS-IDTRANSLOP                                              
046200     MOVE 1 TO WS-START-SLUT-NR                                           
046300     SET IX-SEKVENS TO 1                                                  
046400     PERFORM S01-LAES-W46001                                              
046500                                                                          
046600     PERFORM UNTIL NOT                                                    
046700       (W46001-EOF = NEJ)                                                 
046800                                                                          
046900       MOVE I01-IDPTYP TO TEST-IDPTYP                                     
047000       IF GILTLIG-IDPTYP                                                  
047100                                                                          
047200         PERFORM UNTIL NOT                                                
047300           (W46001-EOF = NEJ AND (I01-IDPTYP =                            
047400              'RH0' OR 'RHA' OR 'RHB' OR 'RHC' OR                         
047500              'RHD' OR 'RHE' OR 'RHF' OR 'RHG' OR 'RHH' OR                
047600              'RHM' OR 'RHN' OR 'RH9'))                                   
047700             MOVE 0 TO WS-ANT-TRANS                                       
047800             MOVE 0 TO WS-KDFEL(IX-SEKVENS)                               
047900             PERFORM CA-KONTROLL-STARTKORT                                
048000             PERFORM CB-KONTROLL-ORDER                                    
048100             PERFORM CC-KONTROLL-SLUTKORT                                 
048200             IF WS-SLUTKORT = NEJ AND WS-KDFEL(IX-SEKVENS) = 0            
048300                 MOVE 17 TO WS-KDFEL(IX-SEKVENS)                          
048400             END-IF                                                       
048500             SET IX-SEKVENS UP BY 1                                       
048600             ADD 1 TO WS-START-SLUT-NR                                    
048700         END-PERFORM                                                      
048800                                                                          
048900         MOVE NEJ TO WS-SLUTKORT                                          
049000                                                                          
049100       ELSE                                                               
049200         ADD 1 TO WS-ANT-TRANS                                            
049300         PERFORM S01-LAES-W46001                                          
049400       END-IF                                                             
049500                                                                          
049600     END-PERFORM                                                          
049700     .                                                                    
049800     EJECT                                                                
049900 CA-KONTROLL-STARTKORT SECTION.                                           
050000     SKIP2                                                                
050100*                        KONTROLLERA ATT STARTKORT FINNS                  
050200     IF I01-IDPTYP = 'RH0'                                                
050300         MOVE I01-START-IDDISTR  TO U08-OHUV-SORT-IDDISTR                 
050400                                    SORTWS-OHUV-SORT-IDDISTR              
050500                                    U1S-KRED-SORT-IDDISTR                 
050600                                    WS-BYTES-IDDISTR                      
050700         MOVE I01-START-TIFILDAT TO WS-TIFILDAT                           
050800                                    U08-OHUV-SORT-TIFILDAT                
050900                                    SORTWS-OHUV-SORT-TIFILDAT             
051000                                    U1S-KRED-SORT-TIFILDAT                
051100         MOVE I01-START-TIHHMMSS TO WS-TIHHMMSS                           
051200                                    U08-OHUV-SORT-TIHHMMSS                
051300                                    U1S-KRED-SORT-TIHHMMSS                
051400                                    SORTWS-OHUV-SORT-TIHHMMSS             
051500                                                                          
051600         PERFORM S01-LAES-W46001                                          
051700                                                                          
051800*                        KONTROLLERA OM DET FINNS DUBBLA STARTKORT        
051900         IF W46001-EOF = NEJ AND WS-KDFEL(IX-SEKVENS) = 0 AND             
052000                I01-IDPTYP = 'RH0'                                        
052100             MOVE 19 TO WS-KDFEL(IX-SEKVENS)                              
052200             PERFORM S01-LAES-W46001                                      
052300         END-IF                                                           
052400                                                                          
052500     ELSE                                                                 
052600         MOVE ZERO TO U08-OHUV-SORT-IDDISTR                               
052700                      U1S-KRED-SORT-IDDISTR                               
052800                      SORTWS-OHUV-SORT-IDDISTR                            
052900                      U08-OHUV-SORT-TIFILDAT                              
053000                      U1S-KRED-SORT-TIFILDAT                              
053100                      SORTWS-OHUV-SORT-TIFILDAT                           
053200                      U08-OHUV-SORT-TIHHMMSS                              
053300                      U1S-KRED-SORT-TIHHMMSS                              
053400                      SORTWS-OHUV-SORT-TIHHMMSS                           
053500         MOVE 14 TO WS-KDFEL(IX-SEKVENS)                                  
053600     END-IF                                                               
053700                                                                          
053800     MOVE ZERO     TO JMF-IDDISTR                                         
053900                      JMF-IDKUNDNR                                        
054000                      JMF-IDORDNR                                         
054100     MOVE SPACE    TO JMF-IDPTYP                                          
054200     .                                                                    
054300     EJECT                                                                
054400 CB-KONTROLL-ORDER SECTION.                                               
054500                                                                          
054600     PERFORM UNTIL NOT                                                    
054700       (W46001-EOF = NEJ AND (I01-IDPTYP =                                
054800             'RHA' OR 'RHB' OR 'RHC' OR 'RHD' OR 'RHE' OR 'RHF'           
054900                   OR 'RHG' OR 'RHH' OR 'RHM' OR 'RHN'))                  
055000         ADD 1 TO WS-ANT-TRANS                                            
055100                                                                          
055200         EVALUATE TRUE                                                    
055300         WHEN  I01-IDPTYP = 'RHG'                                         
055400             PERFORM S17-SKRIV-W46010-RHG                                 
055500         WHEN  I01-IDPTYP = 'RHH'                                         
055600             PERFORM S18-SKRIV-W46010-RHH                                 
055700         WHEN  I01-IDPTYP = 'RHM'                                         
055800             PERFORM S19-SKRIV-W46011-RHM                                 
055900         WHEN  I01-IDPTYP = 'RHN'                                         
056000             PERFORM S20-SKRIV-W46012-RHN                                 
056100         WHEN  I01-IDPTYP = 'RHD' OR 'RHE' OR 'RHF' OR                    
056200                            'RHG' OR 'RHH'                                
056300            EVALUATE I01-IDPTYP                                           
056400            WHEN  'RHD'                                                   
056500               MOVE I01-KRED-IDDISTR TO TEST-IDDISTR                      
056600                                        DIS1-IDDISTR                      
056700            WHEN  'RHE'                                                   
056800               MOVE I01-R15-KRED-IDDISTR TO TEST-IDDISTR                  
056900                                            DIS1-IDDISTR                  
057000            WHEN  'RHF'                                                   
057100               MOVE I01-RHF-KRED-IDDISTR TO TEST-IDDISTR                  
057200                                            DIS1-IDDISTR                  
057300                                            DIS1-IDDISTR                  
057400            WHEN  'RHG'                                                   
057500               MOVE RHG-IDDISTR          TO TEST-IDDISTR                  
057600                                            DIS1-IDDISTR                  
057700            WHEN  'RHH'                                                   
057800               MOVE RHG-IDDISTR          TO TEST-IDDISTR                  
057900                                            DIS1-IDDISTR                  
058000            END-EVALUATE                                                  
058100     EJECT                                                                
058200            CALL W460DIS1 USING DIS1-W460DIS1                             
058300                                                                          
058400            IF (DIS1-KDSVAR = JA OR DIS130-NOAC                           
058500            OR DIS100-FINLLEVANM                                          
058600            OR DIS100-JAPANLEVANM                                         
058700            OR DIS100-ITALLEVANM-PV                                       
058800            OR DIS100-DANMLEVANM-PV                                       
058900            OR DIS100-FRANLEVANM-PV                                       
059000            OR DIS100-SPANLEVANM-PV                                       
059100            OR DIS100-NORGLEVANM-PV                                       
059200            OR DIS100-HOLLLEVANM-PV)                                      
059300               ADD 1 TO WS-IDTRANSLOP                                     
059400               MOVE SPACE    TO U29-R14-R14                               
059500               PERFORM S51-FLYTTA-KRED-POSTER                             
059600               PERFORM S15-SKRIV-W46029                                   
059700                                                                          
059800               EVALUATE I01-IDPTYP                                        
059900               WHEN  'RHD'                                                
060000                  PERFORM S61-FLYTTA-KRED-SUMMAPOST                       
060100               WHEN  'RHE'                                                
060200                  PERFORM S62-FLYTTA-KRED-SUMMAPOST                       
060300               WHEN  'RHF'                                                
060400                  PERFORM S63-FLYTTA-KRED-SUMMAPOST                       
060500               END-EVALUATE                                               
060600                                                                          
060700               PERFORM S16-SKRIV-W4601S                                   
060800            END-IF                                                        
060900         WHEN OTHER                                                       
061000             ADD 1 TO WS-IDTRANSLOP                                       
061100             PERFORM S32-FLYTTA-ORDER-TILL-SORTWS                         
061200             MOVE WS-START-SLUT-NR TO SORTWS-START-SLUT-NR                
061300             MOVE WS-IDTRANSLOP    TO SORTWS-IDTRANSLOP                   
061400             PERFORM CBA-KONTROLL-RHA-RHB-RHC                             
061500             PERFORM S21-SORT-RELEASE                                     
061600             PERFORM S31-FLYTTA-ORDER-TILL-U08                            
061700             PERFORM S13-SKRIV-W46008                                     
061800         END-EVALUATE                                                     
061900                                                                          
062000         PERFORM S01-LAES-W46001                                          
062100     END-PERFORM                                                          
062200     .                                                                    
062300     EJECT                                                                
062400                                                                          
062500 CBA-KONTROLL-RHA-RHB-RHC SECTION.                                        
062600                                                                          
062700     IF WS-KDFEL (IX-SEKVENS) = 0                                         
062800*                        KONTROLLERA OM FLERA ORDERHUVUD                  
062900        IF I01-IDPTYP = 'RHA'                                             
063000           MOVE NEJ           TO ORDER-FINNS-REDAN-SW                     
063100           IF JMF-IDDISTR  = SORTWS-OHUV-IDDISTR  AND                     
063200              JMF-IDKUNDNR = SORTWS-OHUV-IDKUNDNR AND                     
063300              JMF-IDORDNR  = SORTWS-OHUV-IDORDNR                          
063400              MOVE 04 TO WS-KDFEL(IX-SEKVENS)                             
063500           ELSE                                                           
063600*                        KONTROLLERA ATT HASHTOTAL/RHC EJ SAKNAS          
063700              IF JMF-IDPTYP NOT = 'RHC' AND                               
063800                 JMF-IDPTYP NOT = SPACE                                   
063900*                        KONTROLL OM DISTRIKT SKALL HA HASHTOTAL          
064000                 MOVE JMF-IDDISTR TO TEST-IDDISTR                         
064100                 IF DIST61-HASH-TOTAL-IDDISTR                             
064200                    MOVE 18 TO WS-KDFEL(IX-SEKVENS)                       
064300                 END-IF                                                   
064400              ELSE                                                        
064500*                        KONTROLLERA ATT RADER FANNS PÅ FÖREG ORDE        
064600                 IF JMF-IDPTYP = 'RHA'                                    
064700                    MOVE 03 TO WS-KDFEL(IX-SEKVENS)                       
064800                 END-IF                                                   
064900              END-IF                                                      
065000              MOVE SORTWS-OHUV-IDDISTR     TO JMF-IDDISTR                 
065100              MOVE SORTWS-OHUV-IDKUNDNR    TO JMF-IDKUNDNR                
065200              MOVE SORTWS-OHUV-IDORDNR     TO JMF-IDORDNR                 
065300              MOVE SORTWS-OHUV-IDPTYP      TO JMF-IDPTYP                  
065400              MOVE ZERO TO WS-HASH-SUM                                    
065500           END-IF                                                         
065600        END-IF                                                            
065700     END-IF                                                               
065800     EJECT                                                                
065900                                                                          
066000     IF WS-KDFEL (IX-SEKVENS) = 0                                         
066100        IF I01-IDPTYP = 'RHB'                                             
066200           IF JMF-IDDISTR  = SORTWS-ORAD-IDDISTR  AND                     
066300              JMF-IDKUNDNR = SORTWS-ORAD-IDKUNDNR AND                     
066400              JMF-IDORDNR  = SORTWS-ORAD-IDORDNR                          
066500*                        ORDERN FINNS REDAN, RADEN FELMARKERAS            
066600              IF ORDER-FINNS-REDAN                                        
066700                MOVE 09 TO SORTWS-ORAD-KDFEL                              
066800              END-IF                                                      
066900                                                                          
067000*                        KONTROLLERA ATT ARTNR, KONTROLLSIFFRA            
067100*                                    OCH ANTAL ÄR NUMERISKT               
067200              IF SORTWS-ORAD-IDARTNR  NUMERIC AND                         
067300                 SORTWS-ORAD-REKSIFFR NUMERIC AND                         
067400                 SORTWS-ORAD-KVBEART  NUMERIC                             
067500                                                                          
067600*                        ADDERA TILL HASHTOTAL                            
067700                 COMPUTE WS-HASH-SUM = WS-HASH-SUM +                      
067800                         SORTWS-ORAD-IDARTNR +                            
067900                         SORTWS-ORAD-REKSIFFR +                           
068000                         SORTWS-ORAD-KVBEART                              
068100              ELSE                                                        
068200                 MOVE 12 TO WS-KDFEL(IX-SEKVENS)                          
068300              END-IF                                                      
068400                                                                          
068500              MOVE SORTWS-ORAD-IDPTYP TO JMF-IDPTYP                       
068600                                                                          
068700           ELSE                                                           
068800*                        ORDERHUVUD SAKNAS FÖR ORDERN                     
068900              MOVE 05 TO WS-KDFEL(IX-SEKVENS)                             
069000           END-IF                                                         
069100        END-IF                                                            
069200     END-IF                                                               
069300     EJECT                                                                
069400                                                                          
069500     IF WS-KDFEL (IX-SEKVENS) = 0                                         
069600        IF I01-IDPTYP = 'RHC'                                             
069700*                        KONTROLLERA ATT RADER FANNS PÅ ORDERN            
069800           IF JMF-IDPTYP NOT = 'RHB'                                      
069900              MOVE 03 TO WS-KDFEL(IX-SEKVENS)                             
070000           ELSE                                                           
070100*             OM ORDERN REDAN FINNS FELMARKERAS HASHTOTAL-RADEN           
070200              IF ORDER-FINNS-REDAN                                        
070300                 MOVE 09 TO SORTWS-OHASH-KDFEL                            
070400              END-IF                                                      
070500*                        KONTROLL OM DISTRIKT SKALL HA HASHTOTAL          
070600              MOVE JMF-IDDISTR TO TEST-IDDISTR                            
070700              IF DIST61-HASH-TOTAL-IDDISTR                                
070800                                                                          
070900*                        KONTROLL AV HASHTOTAL                            
071000                 IF SORTWS-OHASH-SUHASH NUMERIC                           
071100                    IF WS-HASH-SUM NOT = SORTWS-OHASH-SUHASH              
071200                       MOVE WS-HASH-SUM TO                                
071300                            SORTWS-OHASH-SUHASH-RAETT                     
071400                       MOVE 01 TO WS-KDFEL(IX-SEKVENS)                    
071500                    END-IF                                                
071600                 ELSE                                                     
071700                    MOVE WS-HASH-SUM TO                                   
071800                         SORTWS-OHASH-SUHASH-RAETT                        
071900                    MOVE 13 TO WS-KDFEL(IX-SEKVENS)                       
072000                 END-IF                                                   
072100              ELSE                                                        
072200*                        DISTRIKT SKALL HA EJ HA HASHTOTAL                
072300                 MOVE 21 TO WS-KDFEL(IX-SEKVENS)                          
072400              END-IF                                                      
072500           END-IF                                                         
072600           MOVE SORTWS-ORAD-IDPTYP TO JMF-IDPTYP                          
072700        END-IF                                                            
072800     END-IF                                                               
072900     .                                                                    
073000     EJECT                                                                
073100                                                                          
073200 CC-KONTROLL-SLUTKORT SECTION.                                            
073300     SKIP2                                                                
073400     IF WS-KDFEL (IX-SEKVENS) = 0                                         
073500*                     KONTROLLERA ATT RADER FANNS PÅ ORDERN               
073600        IF JMF-IDPTYP NOT = 'RHB' AND                                     
073700           JMF-IDPTYP NOT = 'RHC'                                         
073800           MOVE 03 TO WS-KDFEL(IX-SEKVENS)                                
073900        END-IF                                                            
074000     END-IF                                                               
074100     IF WS-KDFEL (IX-SEKVENS) = 0                                         
074200*                     KONTROLLERA ATT HASHTOTAL EJ SAKNAS                 
074300        IF JMF-IDPTYP NOT = 'RHC'                                         
074400*                        KONTROLL OM DISTRIKT SKALL HA HASHTOTAL          
074500           MOVE JMF-IDDISTR TO TEST-IDDISTR                               
074600           IF DIST61-HASH-TOTAL-IDDISTR                                   
074700              MOVE 18 TO WS-KDFEL(IX-SEKVENS)                             
074800           END-IF                                                         
074900        END-IF                                                            
075000     END-IF                                                               
075100                                                                          
075200*                        KONTROLLERA OM DET FINNS SLUTKORT                
075300     IF W46001-EOF = NEJ AND I01-IDPTYP = 'RH9'                           
075400         IF WS-KDFEL(IX-SEKVENS) = 0                                      
075500             MOVE JA TO WS-SLUTKORT                                       
075600*                        KONTROLLERA OM ANTALET TRANSAR STÄMMER           
075700             IF WS-ANT-TRANS NOT = I01-SLUT-KVTRANS                       
075800                 MOVE 16 TO WS-KDFEL(IX-SEKVENS)                          
075900             END-IF                                                       
076000         END-IF                                                           
076100         PERFORM S01-LAES-W46001                                          
076200     ELSE                                                                 
076300         MOVE 17 TO WS-KDFEL(IX-SEKVENS)                                  
076400     END-IF                                                               
076500     .                                                                    
076600     EJECT                                                                
076700 D-BEARBETA SECTION.                                                      
076800     SKIP2                                                                
076900                                                                          
077000     PERFORM S22-SORT-RETURN                                              
077100                                                                          
077200     PERFORM UNTIL NOT                                                    
077300       (SORTFIL-EOF = NEJ)                                                
077400         IF SORTWS-OHUV-IDPTYP = 'RHA'                                    
077500            PERFORM DD-DUBBLETT-KONTROLL                                  
077600         END-IF                                                           
077700                                                                          
077800         IF WS-KDFEL(SORTWS-START-SLUT-NR) > 0 OR                         
077900            (SORTWS-OHUV-IDPTYP  = 'RHA'  AND                             
078000             SORTWS-OHUV-KDFEL   > 0)     OR                              
078100            (SORTWS-ORAD-IDPTYP  = 'RHB'  AND                             
078200             SORTWS-ORAD-KDFEL   > 0)     OR                              
078300            (SORTWS-OHASH-IDPTYP = 'RHC'  AND                             
078400             SORTWS-OHASH-KDFEL  > 0)                                     
078500            PERFORM DA-FLYTTA-SORT-TILL-U09                               
078600            PERFORM S14-SKRIV-W46009                                      
078700         ELSE                                                             
078800            IF SORTWS-OHUV-IDPTYP = 'RHA' OR 'RHB'                        
078900               PERFORM DB-FLYTTA-SORT-TILL-U06                            
079000               PERFORM S11-SKRIV-W46006                                   
079100            END-IF                                                        
079200         END-IF                                                           
079300                                                                          
079400         PERFORM S22-SORT-RETURN                                          
079500     END-PERFORM                                                          
079600     .                                                                    
079700     EJECT                                                                
079800 DA-FLYTTA-SORT-TILL-U09 SECTION.                                         
079900     SKIP2                                                                
080000     EVALUATE SORTWS-OHUV-IDPTYP                                          
080100     WHEN  'RHA'                                                          
080200            MOVE SORTWS-OHUV-W460001  TO U09-OHUV-W460001                 
080300            IF WS-KDFEL (SORTWS-START-SLUT-NR) > 0                        
080400               MOVE WS-KDFEL(SORTWS-START-SLUT-NR) TO                     
080500                                         U09-OHUV-KDFEL                   
080600            ELSE                                                          
080700               MOVE SORTWS-OHUV-KDFEL TO U09-OHUV-KDFEL                   
080800            END-IF                                                        
080900     WHEN  'RHB'                                                          
081000            MOVE SORTWS-ORAD-W460002      TO U09-ORAD-W460002             
081100            IF WS-KDFEL (SORTWS-START-SLUT-NR) > 0                        
081200               MOVE WS-KDFEL(SORTWS-START-SLUT-NR) TO                     
081300                                         U09-ORAD-KDFEL                   
081400            ELSE                                                          
081500               MOVE SORTWS-ORAD-KDFEL TO U09-ORAD-KDFEL                   
081600            END-IF                                                        
081700     WHEN  'RHC'                                                          
081800            MOVE SORTWS-OHASH-W460003 TO U09-OHASH-W460003                
081900            IF WS-KDFEL (SORTWS-START-SLUT-NR) > 0                        
082000               MOVE WS-KDFEL(SORTWS-START-SLUT-NR) TO                     
082100                                          U09-OHASH-KDFEL                 
082200            ELSE                                                          
082300               MOVE SORTWS-OHASH-KDFEL TO U09-OHASH-KDFEL                 
082400            END-IF                                                        
082500     WHEN OTHER                                                           
082600            DISPLAY 'FEL IDPTYP ' SORTWS-OHUV-IDPTYP                      
082700     END-EVALUATE                                                         
082800     .                                                                    
082900     EJECT                                                                
083000 DB-FLYTTA-SORT-TILL-U06 SECTION.                                         
083100     SKIP2                                                                
083200     EVALUATE SORTWS-OHUV-IDPTYP                                          
083300     WHEN  'RHA'                                                          
083400            MOVE SORTWS-OHUV-W460001  TO U06-OHUV-W460001                 
083500     WHEN  'RHB'                                                          
083600            MOVE SORTWS-ORAD-W460002      TO U06-ORAD-W460002             
083700     END-EVALUATE                                                         
083800     .                                                                    
083900     EJECT                                                                
084000 DD-DUBBLETT-KONTROLL SECTION.                                            
084100     SKIP2                                                                
084200     IF SORTWS-OHUV-SORT-IDDISTR  = SPAR-OHUV-SORT-IDDISTR  AND           
084300        SORTWS-OHUV-SORT-TIFILDAT = SPAR-OHUV-SORT-TIFILDAT AND           
084400        SORTWS-OHUV-SORT-TIHHMMSS = SPAR-OHUV-SORT-TIHHMMSS AND           
084500        SORTWS-OHUV-IDDISTR       = SPAR-OHUV-IDDISTR  AND                
084600        SORTWS-OHUV-IDKUNDNR      = SPAR-OHUV-IDKUNDNR AND                
084700        SORTWS-OHUV-IDORDNR       = SPAR-OHUV-IDORDNR  AND                
084800        SORTWS-OHUV-IDPTYP        = SPAR-OHUV-IDPTYP                      
084900        IF WS-KDFEL (SORTWS-START-SLUT-NR) = 0                            
085000           MOVE 4   TO WS-KDFEL (SORTWS-START-SLUT-NR)                    
085100        END-IF                                                            
085200     END-IF                                                               
085300     MOVE SORTWS-OHUV-SORT-IDDISTR  TO SPAR-OHUV-SORT-IDDISTR             
085400     MOVE SORTWS-OHUV-SORT-TIFILDAT TO SPAR-OHUV-SORT-TIFILDAT            
085500     MOVE SORTWS-OHUV-SORT-TIHHMMSS TO SPAR-OHUV-SORT-TIHHMMSS            
085600     MOVE SORTWS-OHUV-IDDISTR       TO SPAR-OHUV-IDDISTR                  
085700     MOVE SORTWS-OHUV-IDKUNDNR      TO SPAR-OHUV-IDKUNDNR                 
085800     MOVE SORTWS-OHUV-IDORDNR       TO SPAR-OHUV-IDORDNR                  
085900     MOVE SORTWS-OHUV-IDPTYP        TO SPAR-OHUV-IDPTYP                   
086000     .                                                                    
086100     EJECT                                                                
086200 S01-LAES-W46001 SECTION.                                                 
086300     SKIP2                                                                
086400     READ W46001 INTO I01-AREA2                                           
086500         AT END                                                           
086600             MOVE JA TO W46001-EOF                                        
086700     END-READ                                                             
086800*                                                                         
086900     IF W46001-EOF = NEJ                                                  
087000         IF I01-REQU-IDMSGVER NUMERIC                                     
087100           MOVE I01-AREAREST      TO I01-AREA                             
087200           MOVE I01-REQU-IDMSGVER TO SPAR-REQU-IDMSGVER                   
087300           MOVE I01-REQU-KDPGMACT TO SPAR-REQU-KDPGMACT                   
087400           MOVE I01-REQU-IDUSER   TO SPAR-REQU-IDUSER                     
087500         ELSE                                                             
087600           MOVE I01-AREA2         TO I01-AREA                             
087700           MOVE SPACE             TO SPAR-REQU-IDMSGVER                   
087800           MOVE SPACE             TO SPAR-REQU-KDPGMACT                   
087900           MOVE SPACE             TO SPAR-REQU-IDUSER                     
088000         END-IF                                                           
088010           MOVE I01-REQU-KDPGMACT TO SPAR-REQU-KDPGMACT                   
088020           MOVE I01-REQU-IDUSER   TO SPAR-REQU-IDUSER                     
088100                                                                          
088200         MOVE 'W46001' TO POSTSUM-FDNAMN                                  
088300         MOVE 'W46005D1' TO POSTSUM-DDNAMN2                               
088400         MOVE I01-IDPTYP TO POSTSUM-TRANSTYP                              
088500         CALL POSTSUM USING POSTSUM-PARM                                  
088600     END-IF                                                               
088700     .                                                                    
088800     EJECT                                                                
088900 S11-SKRIV-W46006 SECTION.                                                
089000     SKIP2                                                                
089100     EVALUATE U06-OHUV-IDPTYP                                             
089200     WHEN 'RHA'                                                           
089300               WRITE W46006-W460001 FROM U06-OHUV-W460001                 
089400     WHEN 'RHB'                                                           
089500               WRITE W46006-W460002 FROM U06-ORAD-W460002                 
089600     END-EVALUATE                                                         
089700     MOVE 'W46006' TO POSTSUM-FDNAMN                                      
089800     MOVE 'W46005D3' TO POSTSUM-DDNAMN2                                   
089900     MOVE U06-OHUV-IDPTYP TO POSTSUM-TRANSTYP                             
090000     CALL POSTSUM USING POSTSUM-PARM                                      
090100     .                                                                    
090200     EJECT                                                                
090300 S13-SKRIV-W46008 SECTION.                                                
090400     SKIP2                                                                
090500     EVALUATE U08-OHUV-IDPTYP                                             
090600     WHEN 'RHA'                                                           
090700               WRITE W46008-W460001 FROM U08-OHUV-W460001                 
090800     WHEN 'RHB'                                                           
090900               WRITE W46008-W460002 FROM U08-ORAD-W460002                 
091000     WHEN 'RHC'                                                           
091100               WRITE W46008-W460003 FROM U08-OHASH-W460003                
091200     END-EVALUATE                                                         
091300     MOVE 'W46008' TO POSTSUM-FDNAMN                                      
091400     MOVE 'W46005D5' TO POSTSUM-DDNAMN2                                   
091500     MOVE U08-OHUV-IDPTYP TO POSTSUM-TRANSTYP                             
091600     CALL POSTSUM USING POSTSUM-PARM                                      
091700     .                                                                    
091800     EJECT                                                                
091900 S14-SKRIV-W46009 SECTION.                                                
092000     SKIP2                                                                
092100     EVALUATE U09-OHUV-IDPTYP                                             
092200     WHEN 'RHA'                                                           
092300               WRITE W46009-W460001 FROM U09-OHUV-W460001                 
092400     WHEN 'RHB'                                                           
092500               WRITE W46009-W460002 FROM U09-ORAD-W460002                 
092600     WHEN 'RHC'                                                           
092700               WRITE W46009-W460003 FROM U09-OHASH-W460003                
092800     WHEN OTHER                                                           
092900               DISPLAY 'FEL IDPTYP ' I01-IDPTYP                           
093000     END-EVALUATE                                                         
093100     MOVE 'W46009' TO POSTSUM-FDNAMN                                      
093200     MOVE 'W46005D6' TO POSTSUM-DDNAMN2                                   
093300     MOVE U09-OHUV-IDPTYP TO POSTSUM-TRANSTYP                             
093400     CALL POSTSUM USING POSTSUM-PARM                                      
093500     .                                                                    
093600     EJECT                                                                
093700 S15-SKRIV-W46029 SECTION.                                                
093800     SKIP2                                                                
093900     WRITE W46029-W418R14 FROM U29-R14-W418R14                            
094000     SKIP2                                                                
094100     MOVE 'W46029' TO POSTSUM-FDNAMN                                      
094200     MOVE 'W46005D7' TO POSTSUM-DDNAMN2                                   
094300     MOVE U29-R14-IDPTYP TO POSTSUM-TRANSTYP                              
094400     CALL POSTSUM USING POSTSUM-PARM                                      
094500     .                                                                    
094600     EJECT                                                                
094700 S16-SKRIV-W4601S SECTION.                                                
094800     SKIP2                                                                
094900     WRITE W4601S-W460006 FROM U1S-KRED-W460006                           
095000     SKIP2                                                                
095100     MOVE 'W4601S' TO POSTSUM-FDNAMN                                      
095200     MOVE 'W46005D8' TO POSTSUM-DDNAMN2                                   
095300     MOVE U1S-KRED-IDPTYP TO POSTSUM-TRANSTYP                             
095400     CALL POSTSUM USING POSTSUM-PARM                                      
095500     .                                                                    
095600     EJECT                                                                
095700 S17-SKRIV-W46010-RHG SECTION.                                            
095800     SKIP2                                                                
095900     WRITE W46010-KUND-POST   FROM RHG-W460RHG                            
096000     SKIP2                                                                
096100     MOVE 'W46010' TO POSTSUM-FDNAMN                                      
096200     MOVE 'W46005D9' TO POSTSUM-DDNAMN2                                   
096300     MOVE 'KUND'          TO POSTSUM-TRANSTYP                             
096400     CALL POSTSUM USING POSTSUM-PARM                                      
096500     .                                                                    
096600     EJECT                                                                
096700 S18-SKRIV-W46010-RHH SECTION.                                            
096800     SKIP2                                                                
096900     WRITE W46010-KUND-POST   FROM RHH-W460RHH                            
097000     SKIP2                                                                
097100     MOVE 'W46010' TO POSTSUM-FDNAMN                                      
097200     MOVE 'W46005D9' TO POSTSUM-DDNAMN2                                   
097300     MOVE 'KUND'          TO POSTSUM-TRANSTYP                             
097400     CALL POSTSUM USING POSTSUM-PARM                                      
097500     .                                                                    
097600     EJECT                                                                
097700 S19-SKRIV-W46011-RHM SECTION.                                            
097800     SKIP2                                                                
097900     IF RHM-IDBYTRAD NUMERIC                                              
098000        MOVE RHM-IDBYTRAD      TO W371-IDBYTRAD                           
098100     ELSE                                                                 
098200        MOVE ZERO              TO W371-IDBYTRAD                           
098300     END-IF                                                               
098400     MOVE RHM-IDPTYP           TO W371-IDPTYP                             
098500     MOVE RHM-IDDISTR          TO W371-IDDISTR                            
098600     MOVE RHM-IDGMTREF         TO W371-IDGMTREF                           
098700     MOVE RHM-IDFAKT           TO W371-IDFAKT                             
098800     MOVE RHM-IDBYTRAP         TO W371-IDBYTRAP                           
098900     MOVE RHM-TIREGDAT         TO W371-TIREGDAT                           
099000     MOVE RHM-IDARTNR-OBJ      TO W371-IDARTNR-OBJ                        
099100     MOVE RHM-IDTABNR          TO W371-IDTABNR                            
099200     MOVE RHM-KVRETUR-URSP     TO W371-KVRETUR-URSP                       
099300     MOVE RHM-BERADREF         TO W371-BERADREF                           
099400     MOVE RHM-IDDC             TO W371-IDDC                               
099500     MOVE RHM-FLBYTGAR         TO W371-FLBYTGAR                           
099600     MOVE WS-BYTES-IDDISTR     TO W371-IDDISTR-FEL                        
099700                                                                          
099800                                                                          
099900     WRITE W46011-BYTES-POST  FROM W371-W37116                            
100000     SKIP2                                                                
100100     MOVE 'W46011' TO POSTSUM-FDNAMN                                      
100200     MOVE 'W46005DA' TO POSTSUM-DDNAMN2                                   
100300     MOVE 'RHM '          TO POSTSUM-TRANSTYP                             
100400     CALL POSTSUM USING POSTSUM-PARM                                      
100500     .                                                                    
100600     EJECT                                                                
100700 S20-SKRIV-W46012-RHN SECTION.                                            
100800     SKIP2                                                                
100900     WRITE W46012-OBKR-POST  FROM RHN-W460RHN-CTX                         
101000     SKIP2                                                                
101100     MOVE 'W46012' TO POSTSUM-FDNAMN                                      
101200     MOVE 'W46005DB' TO POSTSUM-DDNAMN2                                   
101300     MOVE 'RHN '          TO POSTSUM-TRANSTYP                             
101400     CALL POSTSUM USING POSTSUM-PARM                                      
101500     .                                                                    
101600     EJECT                                                                
101700 S21-SORT-RELEASE SECTION.                                                
101800     SKIP2                                                                
101900     RELEASE SORT-POST FROM SORTWS-AREA                                   
102000     .                                                                    
102100     EJECT                                                                
102200 S22-SORT-RETURN SECTION.                                                 
102300     SKIP2                                                                
102400     RETURN SORTFIL INTO SORTWS-AREA                                      
102500         AT END                                                           
102600             MOVE JA TO SORTFIL-EOF                                       
102700     END-RETURN                                                           
102800     .                                                                    
102900     EJECT                                                                
103000 S31-FLYTTA-ORDER-TILL-U08 SECTION.                                       
103100     SKIP2                                                                
103200     EVALUATE I01-IDPTYP                                                  
103300     WHEN 'RHA'                                                           
103400               MOVE I01-OHUV-IDPTYP      TO U08-OHUV-IDPTYP               
103500               MOVE I01-OHUV-IDDISTR     TO U08-OHUV-IDDISTR              
103600               MOVE I01-OHUV-IDKUNDNR    TO U08-OHUV-IDKUNDNR             
103700               MOVE I01-OHUV-IDORDNR     TO U08-OHUV-IDORDNR              
103800               MOVE I01-OHUV-BEVOLREF    TO U08-OHUV-BEVOLREF             
103900               MOVE I01-OHUV-KDFRAKT     TO U08-OHUV-KDFRAKT              
104000               MOVE I01-OHUV-KDORDKL     TO U08-OHUV-KDORDKL              
104100               MOVE I01-OHUV-KDORDKL-IMP TO U08-OHUV-KDORDKL-IMP          
104200               MOVE I01-OHUV-KDROPACK    TO U08-OHUV-KDROPACK             
104300               MOVE I01-OHUV-KDORDURS    TO U08-OHUV-KDORDURS             
104400               MOVE I01-OHUV-BEVARREF    TO U08-OHUV-BEVARREF             
104500               MOVE I01-OHUV-FLRESTN     TO U08-OHUV-FLRESTN              
104600               MOVE I01-OHUV-KDNCNOT     TO U08-OHUV-KDNCNOT              
104700               MOVE I01-OHUV-KDTPOTYP    TO U08-OHUV-KDTPOTYP             
104800               IF I01-OHUV-IDKAMPRF NUMERIC                               
104900                  MOVE I01-OHUV-IDKAMPRF TO U08-OHUV-IDKAMPRF             
105000               ELSE                                                       
105100                  MOVE ZERO              TO U08-OHUV-IDKAMPRF             
105200               END-IF                                                     
105300               MOVE WS-IDTRANSLOP        TO U08-OHUV-IDTRANSLOP           
105400               MOVE 0                    TO U08-OHUV-KDFEL                
105500               IF SPAR-REQU-IDMSGVER = SPACE                              
105600                 MOVE SPACE              TO U08-OHUV-BEGMT                
105700                                            U08-OHUV-ADGMT                
105800                 MOVE ZERO               TO U08-OHUV-TIBEGPAC             
105900               ELSE                                                       
106000                 MOVE I01-OHUV-BEGMT     TO U08-OHUV-BEGMT                
106100                 MOVE I01-OHUV-ADGMT     TO U08-OHUV-ADGMT                
106200                 MOVE I01-OHUV-TIBEGPAC  TO U08-OHUV-TIBEGPAC             
106300               END-IF                                                     
106310               MOVE SPAR-REQU-IDUSER-GRP TO U08-OHUV-IDBILREG             
106320*                                                                         
106400*SPAR-REQU-IDUSER-GRP CONTAINS THE DATA FOR IDBILREG                      
106500*THIS IS A VIPS FIX TO SEND IDBILREG WITHOUT MAKING THE AREA              
106501*W460001 LONGER. THE W460001 IS ALREADY AT MAX LENGTH.                    
106600      WHEN 'RHB'                                                          
106700               MOVE I01-ORAD-IDPTYP      TO U08-ORAD-IDPTYP               
106800               MOVE I01-ORAD-IDDISTR     TO U08-ORAD-IDDISTR              
106900               MOVE I01-ORAD-IDKUNDNR    TO U08-ORAD-IDKUNDNR             
107000               MOVE I01-ORAD-IDORDNR     TO U08-ORAD-IDORDNR              
107100               MOVE I01-ORAD-IDARTNR     TO U08-ORAD-IDARTNR              
107200               MOVE I01-ORAD-REKSIFFR    TO U08-ORAD-REKSIFFR             
107300               MOVE I01-ORAD-BERADREF    TO U08-ORAD-BERADREF             
107400               MOVE I01-ORAD-KVBEART     TO U08-ORAD-KVBEART              
107500               MOVE I01-ORAD-KDKVBRYT    TO U08-ORAD-KDKVBRYT             
107600               MOVE I01-ORAD-KDDSP       TO U08-ORAD-KDDSP                
107700               MOVE I01-ORAD-TITPO       TO U08-ORAD-TITPO                
107800               MOVE I01-ORAD-FLSLATT     TO U08-ORAD-FLSLATT              
107900               MOVE I01-ORAD-FLDIRLEV    TO U08-ORAD-FLDIRLEV             
108000               MOVE WS-IDTRANSLOP        TO U08-ORAD-IDTRANSLOP           
108100               MOVE 0                    TO U08-ORAD-KDFEL                
108200               MOVE 0                    TO                               
108300                                         U08-ORAD-REKSIFFR-RAETT          
108400               MOVE 0                    TO U08-ORAD-IDPRQUES             
108500               MOVE 0                    TO                               
108600                                       U08-ORAD-PRARTNTO-LOCPREL          
108700                                                                          
108800              IF SPAR-REQU-IDMSGVER = SPACE                               
108900                 MOVE 0                 TO                                
109000                                        U08-ORAD-PRARTNTO-LOC             
109100                 MOVE 0                 TO                                
109200                                        U08-ORAD-PRARTBTO-LOC             
109300                 MOVE SPACE             TO                                
109400                                        U08-ORAD-KDVALISO                 
109500                 MOVE SPACE             TO                                
109600                                        U08-ORAD-KDVAT                    
109700                 MOVE 0                 TO                                
109800                                        U08-ORAD-RERAB                    
109900                 MOVE SPACE             TO                                
110000                                        U08-ORAD-KDRAB                    
110100                 MOVE SPACE             TO                                
110200                                        U08-ORAD-BEART-VIPS               
110300              ELSE                                                        
110400                 MOVE I01-ORAD-PRARTNTO-LOC TO                            
110500                                        U08-ORAD-PRARTNTO-LOC             
110600                 MOVE I01-ORAD-PRARTBTO-LOC TO                            
110700                                        U08-ORAD-PRARTBTO-LOC             
110800                 MOVE I01-ORAD-KDVALISO TO                                
110900                                        U08-ORAD-KDVALISO                 
111000                 MOVE I01-ORAD-KDVAT    TO                                
111100                                        U08-ORAD-KDVAT                    
111200                 MOVE I01-ORAD-RERAB    TO                                
111300                                        U08-ORAD-RERAB                    
111400                 MOVE I01-ORAD-KDRAB    TO                                
111500                                        U08-ORAD-KDRAB                    
111600                 MOVE I01-ORAD-BEART-VIPS TO                              
111700                                        U08-ORAD-BEART-VIPS               
111800              END-IF                                                      
111900      WHEN 'RHC'                                                          
112000               MOVE I01-OHASH-IDPTYP     TO U08-OHASH-IDPTYP              
112100               MOVE I01-OHASH-IDDISTR    TO U08-OHASH-IDDISTR             
112200               MOVE I01-OHASH-IDKUNDNR   TO U08-OHASH-IDKUNDNR            
112300               MOVE I01-OHASH-IDORDNR    TO U08-OHASH-IDORDNR             
112400               MOVE I01-OHASH-SUHASH     TO U08-OHASH-SUHASH              
112500               MOVE WS-IDTRANSLOP        TO U08-OHASH-IDTRANSLOP          
112600               MOVE 0                    TO U08-OHASH-KDFEL               
112700               MOVE 0                    TO U08-OHASH-SUHASH-RAETT        
112800     END-EVALUATE                                                         
112900     .                                                                    
113000     EJECT                                                                
113100 S32-FLYTTA-ORDER-TILL-SORTWS SECTION.                                    
113200     SKIP2                                                                
113300     EVALUATE I01-IDPTYP                                                  
113400     WHEN 'RHA'                                                           
113500               MOVE I01-OHUV-IDPTYP      TO                               
113600                                         SORTWS-OHUV-IDPTYP               
113700               MOVE I01-OHUV-IDDISTR     TO                               
113800                                         SORTWS-OHUV-IDDISTR              
113900               MOVE I01-OHUV-IDKUNDNR    TO                               
114000                                         SORTWS-OHUV-IDKUNDNR             
114100               MOVE I01-OHUV-IDORDNR     TO                               
114200                                         SORTWS-OHUV-IDORDNR              
114300               MOVE I01-OHUV-BEVOLREF    TO                               
114400                                         SORTWS-OHUV-BEVOLREF             
114500               MOVE I01-OHUV-KDFRAKT     TO                               
114600                                         SORTWS-OHUV-KDFRAKT              
114700               MOVE I01-OHUV-KDORDKL     TO                               
114800                                         SORTWS-OHUV-KDORDKL              
114900               MOVE I01-OHUV-KDORDKL-IMP TO                               
115000                                         SORTWS-OHUV-KDORDKL-IMP          
115100               MOVE I01-OHUV-KDROPACK    TO                               
115200                                         SORTWS-OHUV-KDROPACK             
115300               MOVE I01-OHUV-KDORDURS    TO                               
115400                                         SORTWS-OHUV-KDORDURS             
115500               MOVE I01-OHUV-BEVARREF    TO                               
115600                                         SORTWS-OHUV-BEVARREF             
115700               MOVE I01-OHUV-FLRESTN     TO                               
115800                                         SORTWS-OHUV-FLRESTN              
115900               MOVE I01-OHUV-KDNCNOT     TO                               
116000                                         SORTWS-OHUV-KDNCNOT              
116100               MOVE I01-OHUV-KDTPOTYP    TO                               
116200                                         SORTWS-OHUV-KDTPOTYP             
116300               IF I01-OHUV-IDKAMPRF NUMERIC                               
116400                  MOVE I01-OHUV-IDKAMPRF TO                               
116500                                         SORTWS-OHUV-IDKAMPRF             
116600               ELSE                                                       
116700                  MOVE ZERO           TO SORTWS-OHUV-IDKAMPRF             
116800               END-IF                                                     
116900               IF SPAR-REQU-IDMSGVER = SPACE                              
117000                 MOVE SPACE              TO SORTWS-OHUV-BEGMT             
117100                                            SORTWS-OHUV-ADGMT             
117203                 MOVE ZERO               TO SORTWS-OHUV-TIBEGPAC          
117300               ELSE                                                       
117400                 MOVE I01-OHUV-BEGMT     TO SORTWS-OHUV-BEGMT             
117500                 MOVE I01-OHUV-ADGMT     TO SORTWS-OHUV-ADGMT             
117603                 MOVE I01-OHUV-TIBEGPAC  TO SORTWS-OHUV-TIBEGPAC          
117700               END-IF                                                     
117800               MOVE WS-IDTRANSLOP        TO                               
117900                                         SORTWS-OHUV-IDTRANSLOP           
118100               MOVE 0                    TO SORTWS-OHUV-KDFEL             
118110               MOVE SPAR-REQU-IDUSER-GRP TO SORTWS-OHUV-IDBILREG          
118200     WHEN 'RHB'                                                           
118300               MOVE I01-ORAD-IDPTYP      TO                               
118400                                         SORTWS-ORAD-IDPTYP               
118500               MOVE I01-ORAD-IDDISTR     TO                               
118600                                         SORTWS-ORAD-IDDISTR              
118700               MOVE I01-ORAD-IDKUNDNR    TO                               
118800                                         SORTWS-ORAD-IDKUNDNR             
118900               MOVE I01-ORAD-IDORDNR     TO                               
119000                                         SORTWS-ORAD-IDORDNR              
119100               MOVE I01-ORAD-IDARTNR     TO                               
119200                                         SORTWS-ORAD-IDARTNR              
119300               MOVE I01-ORAD-REKSIFFR    TO                               
119400                                         SORTWS-ORAD-REKSIFFR             
119500               MOVE I01-ORAD-BERADREF    TO                               
119600                                         SORTWS-ORAD-BERADREF             
119700               MOVE I01-ORAD-KVBEART     TO                               
119800                                         SORTWS-ORAD-KVBEART              
119900               MOVE I01-ORAD-KDKVBRYT    TO                               
120000                                         SORTWS-ORAD-KDKVBRYT             
120100               MOVE I01-ORAD-KDDSP       TO                               
120200                                         SORTWS-ORAD-KDDSP                
120300               MOVE I01-ORAD-TITPO       TO                               
120400                                         SORTWS-ORAD-TITPO                
120500               MOVE I01-ORAD-FLSLATT     TO                               
120600                                         SORTWS-ORAD-FLSLATT              
120700               MOVE I01-ORAD-FLDIRLEV    TO                               
120800                                         SORTWS-ORAD-FLDIRLEV             
120900               MOVE 0                    TO                               
121000                                       SORTWS-ORAD-REKSIFFR-RAETT         
121100               MOVE WS-IDTRANSLOP        TO                               
121200                                         SORTWS-ORAD-IDTRANSLOP           
121300               MOVE 0                    TO                               
121400                                         SORTWS-ORAD-KDFEL                
121500               MOVE 0                    TO                               
121600                                         SORTWS-ORAD-IDPRQUES             
121700               MOVE 0                    TO                               
121800                                    SORTWS-ORAD-PRARTNTO-LOCPREL          
121900                                                                          
122000              IF SPAR-REQU-IDMSGVER = SPACE                               
122100                 MOVE 0                 TO                                
122200                                        SORTWS-ORAD-PRARTNTO-LOC          
122300                 MOVE 0                 TO                                
122400                                        SORTWS-ORAD-PRARTBTO-LOC          
122500                 MOVE SPACE             TO                                
122600                                        SORTWS-ORAD-KDVALISO              
122700                 MOVE SPACE             TO                                
122800                                        SORTWS-ORAD-KDVAT                 
122900                 MOVE 0                 TO                                
123000                                        SORTWS-ORAD-RERAB                 
123100                 MOVE SPACE             TO                                
123200                                        SORTWS-ORAD-KDRAB                 
123300                 MOVE SPACE             TO                                
123400                                        SORTWS-ORAD-BEART-VIPS            
123500              ELSE                                                        
123600                 MOVE I01-ORAD-PRARTNTO-LOC TO                            
123700                                        SORTWS-ORAD-PRARTNTO-LOC          
123800                 MOVE I01-ORAD-PRARTBTO-LOC TO                            
123900                                        SORTWS-ORAD-PRARTBTO-LOC          
124000                 MOVE I01-ORAD-KDVALISO TO                                
124100                                        SORTWS-ORAD-KDVALISO              
124200                 MOVE I01-ORAD-KDVAT    TO                                
124300                                        SORTWS-ORAD-KDVAT                 
124400                 MOVE I01-ORAD-RERAB    TO                                
124500                                        SORTWS-ORAD-RERAB                 
124600                 MOVE I01-ORAD-KDRAB    TO                                
124700                                        SORTWS-ORAD-KDRAB                 
124800                 MOVE I01-ORAD-BEART-VIPS TO                              
124900                                        SORTWS-ORAD-BEART-VIPS            
125000              END-IF                                                      
125100     WHEN 'RHC'                                                           
125200               MOVE I01-OHASH-IDPTYP     TO                               
125300                                         SORTWS-OHASH-IDPTYP              
125400               MOVE I01-OHASH-IDDISTR    TO                               
125500                                         SORTWS-OHASH-IDDISTR             
125600               MOVE I01-OHASH-IDKUNDNR   TO                               
125700                                         SORTWS-OHASH-IDKUNDNR            
125800               MOVE I01-OHASH-IDORDNR    TO                               
125900                                         SORTWS-OHASH-IDORDNR             
126000               MOVE I01-OHASH-SUHASH     TO                               
126100                                         SORTWS-OHASH-SUHASH              
126200               MOVE WS-IDTRANSLOP        TO                               
126300                                         SORTWS-OHASH-IDTRANSLOP          
126400               MOVE 0                    TO                               
126500                                         SORTWS-OHASH-SUHASH-RAETT        
126600               MOVE 0                    TO                               
126700                                         SORTWS-OHASH-KDFEL               
126800     END-EVALUATE                                                         
126900     .                                                                    
127000     EJECT                                                                
127100 S51-FLYTTA-KRED-POSTER SECTION.                                          
127200     SKIP2                                                                
127300     EVALUATE I01-IDPTYP                                                  
127400     WHEN  'RHD'                                                          
127500        MOVE I01-IDPTYP          TO U29-R14-IDPTYP                        
127600        MOVE I01-IDPTYP          TO U29-R14-IDPTYP                        
127700        MOVE I01-KRED-IDDISTR    TO U29-R14-IDDISTR                       
127800        MOVE I01-KRED-IDKUNDNR   TO U29-R14-IDKUNDNR                      
127900        MOVE I01-KRED-KDCLAGER   TO U29-R14-KDCLAGER                      
128000        IF I01-KRED-KDCLAGER = 6                                          
128100           MOVE 1                TO U29-R14-KDCLAGER                      
128200        END-IF                                                            
128300        MOVE I01-KRED-KDFRAKT    TO U29-R14-KDFRAKT                       
128400        MOVE I01-KRED-IDLEVANM   TO U29-R14-IDLEVANM                      
128500        MOVE I01-KRED-TIM-LEVANM TO U29-R14-TIM-LEVANM                    
128600                                    W-TIM-LEVANM                          
128700        MOVE I01-KRED-IDORDNR    TO U29-R14-IDORDNR                       
128800        MOVE I01-KRED-IDKOLLI    TO U29-R14-IDKOLLI                       
128900        MOVE I01-KRED-IDARTNR    TO U29-R14-IDARTNR                       
129000        MOVE I01-KRED-IDRADNR    TO U29-R14-IDRADNR                       
129100        MOVE I01-KRED-REKSIFFR   TO U29-R14-REKSIFFR                      
129200        MOVE I01-KRED-KVLEVANM   TO U29-R14-KVLEVANM                      
129300        MOVE I01-KRED-KDANMORS   TO U29-R14-KDANMORS                      
129400        MOVE I01-KRED-KDEMBLEV   TO U29-R14-KDEMBLEV                      
129500        MOVE I01-KRED-PRARTBTO   TO U29-R14-PRARTBTO                      
129600        MOVE I01-KRED-KDFAKTYP   TO U29-R14-KDFAKTYP                      
129700        MOVE I01-KRED-IDFAKT     TO U29-R14-IDFAKT                        
129800        MOVE I01-KRED-TIFAKT     TO U29-R14-TIFAKT                        
129900        MOVE I01-KRED-FLDIRLEV   TO U29-R14-FLDIRLEV                      
130000        MOVE I01-KRED-KDSPEKTO   TO U29-R14-KDSPEKTO                      
130100        MOVE I01-KRED-KDFTG      TO U29-R14-KDFTG                         
130200*** FIX FÖR SCHWEIZ SOM ENVISAS MED ATT SKICKA SKRÄP I KONTO              
130300*** OCH FLAGGA SKROT.                                                     
130400        IF I01-KRED-IDKONTO NOT NUMERIC                                   
130500           MOVE ZERO TO I01-KRED-IDKONTO                                  
130600        END-IF                                                            
130700        IF I01-KRED-FLSKROT NOT NUMERIC                                   
130800           MOVE ZERO TO I01-KRED-FLSKROT                                  
130900        END-IF                                                            
131000        MOVE I01-KRED-IDKONTO    TO U29-R14-IDKONTO-RAD                   
131100        MOVE I01-KRED-FLSKROT    TO U29-R14-FLSKROT                       
131200        MOVE ZERO                TO U29-R14-FLSTRET                       
131300        MOVE ZERO                TO U29-R14-KDNIVAA4                      
131400        MOVE ZERO                TO U29-R14-PRARTBTO-LOC                  
131500     WHEN  'RHE'                                                          
131600        MOVE I01-IDPTYP               TO U29-R15-IDPTYP                   
131700        MOVE I01-R15-KRED-IDDISTR     TO U29-R15-IDDISTR                  
131800        MOVE I01-R15-KRED-IDKUNDNR    TO U29-R15-IDKUNDNR                 
131900        MOVE I01-R15-KRED-KDCLAGER    TO U29-R15-KDCLAGER                 
132000        IF I01-R15-KRED-KDCLAGER = 6                                      
132100           MOVE 1                     TO U29-R15-KDCLAGER                 
132200        END-IF                                                            
132300        MOVE I01-R15-KRED-IDLEVANM    TO U29-R15-IDLEVANM                 
132400        MOVE I01-R15-KRED-KDFRAKT     TO U29-R15-KDFRAKT                  
132500        MOVE W-TIM-LEVANM             TO U29-R15-TIM-LEVANM               
132600        MOVE I01-R15-KRED-RELANDCO    TO U29-R15-RELANDCO                 
132700        MOVE I01-R15-KRED-PREMBHNT    TO U29-R15-PREMBHNT                 
132800        MOVE I01-R15-KRED-PRFRAKT     TO U29-R15-PRFRAKT                  
132900        MOVE I01-R15-KRED-PRLEGKST    TO U29-R15-PRLEGKST                 
133000        MOVE I01-R15-KRED-PRFOERS     TO U29-R15-PRFOERS                  
133100        MOVE I01-R15-KRED-PREXPKST    TO U29-R15-PREXPKST                 
133200        MOVE I01-R15-KRED-PRMOMS      TO U29-R15-PRMOMS                   
133300     WHEN  'RHF'                                                          
133400        MOVE I01-IDPTYP               TO U29-RHF-IDPTYP                   
133500        MOVE I01-RHF-KRED-IDDISTR     TO U29-RHF-IDDISTR                  
133600        MOVE I01-RHF-KRED-IDKUNDNR    TO U29-RHF-IDKUNDNR                 
133700        MOVE I01-RHF-KRED-KDCLAGER    TO U29-RHF-KDCLAGER                 
133800        IF I01-RHF-KRED-KDCLAGER = 6                                      
133900           MOVE 1                     TO U29-RHF-KDCLAGER                 
134000        END-IF                                                            
134100        MOVE I01-RHF-KRED-IDLEVANM    TO U29-RHF-IDLEVANM                 
134200        MOVE I01-RHF-KRED-TEKRENOT    TO U29-RHF-TEKRENOT                 
134300     END-EVALUATE                                                         
134400     .                                                                    
134500     EJECT                                                                
134600 S61-FLYTTA-KRED-SUMMAPOST SECTION.                                       
134700                                                                          
134800     MOVE 'RHD'               TO U1S-KRED-IDPTYP                          
134900     MOVE I01-KRED-IDDISTR    TO U1S-KRED-IDDISTR                         
135000                                 TEST-IDDISTR                             
135100     MOVE I01-KRED-IDKUNDNR   TO U1S-KRED-IDKUNDNR                        
135200     MOVE I01-KRED-IDORDNR    TO U1S-KRED-IDORDNR                         
135300     MOVE WS-IDTRANSLOP       TO U1S-KRED-IDTRANSLOP                      
135400     .                                                                    
135500     EJECT                                                                
135600 S62-FLYTTA-KRED-SUMMAPOST SECTION.                                       
135700                                                                          
135800     MOVE 'RHE'                  TO U1S-KRED-IDPTYP                       
135900     MOVE I01-R15-KRED-IDDISTR   TO U1S-KRED-IDDISTR                      
136000     MOVE I01-R15-KRED-IDKUNDNR  TO U1S-KRED-IDKUNDNR                     
136100     MOVE ZERO                   TO U1S-KRED-IDORDNR                      
136200     MOVE WS-IDTRANSLOP          TO U1S-KRED-IDTRANSLOP                   
136300     .                                                                    
136400     EJECT                                                                
136500 S63-FLYTTA-KRED-SUMMAPOST SECTION.                                       
136600                                                                          
136700     MOVE 'RHF'                  TO U1S-KRED-IDPTYP                       
136800     MOVE I01-RHF-KRED-IDDISTR   TO U1S-KRED-IDDISTR                      
136900     MOVE I01-RHF-KRED-IDKUNDNR  TO U1S-KRED-IDKUNDNR                     
137000     MOVE ZERO                   TO U1S-KRED-IDORDNR                      
137100     MOVE WS-IDTRANSLOP          TO U1S-KRED-IDTRANSLOP                   
137200     .                                                                    
137300     EJECT                                                                
137400 IMS-STATUSKONTROLL SECTION.                                              
137500     SET STATUS-IX TO 1                                                   
137600     SEARCH GODK-STATUS                                                   
137700          AT END CALL FELLOG                                              
137800     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
137900          CONTINUE                                                        
138000     .                                                                    
138100     EJECT                                                                
138200 Z-FINIT SECTION.                                                         
138300     SKIP2                                                                
138400     CLOSE W46001                                                         
138500           W46006                                                         
138600           W46008                                                         
138700           W46009                                                         
138800           W46029                                                         
138900           W4601S                                                         
139000           W46010                                                         
139100           W46011                                                         
139200           W46012                                                         
139300     SKIP2                                                                
139400     MOVE 'S' TO POSTSUM-OPKOD                                            
139500     CALL POSTSUM USING POSTSUM-PARM                                      
140000     .                                                                    
