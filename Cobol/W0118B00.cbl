000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W0118B00.                                                
000400 AUTHOR.         PRIYA RC                                                 
000500 DATE-WRITTEN.   24/10/09                                                 
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        READ W01184 AND CONVERT IT INTO TAB DELIMITED FORMAT             
001000*        LDC                                                              
001100*        SDC                                                              
001200*        NDC                                                              
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500     SELECT XDC-W01184                 ASSIGN TO W0118BD1.                
003600                                                                          
003700     SELECT LDC-W01184EX               ASSIGN TO W0118BD2.                
003800                                                                          
003900     SELECT SDC-W01184EX               ASSIGN TO W0118BD3.                
004000                                                                          
004100     SELECT NDC-W01184EX               ASSIGN TO W0118BD4.                
004200                                                                          
004300 DATA DIVISION.                                                           
004400     SKIP2                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  XDC-W01184                                                           
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
006000*01  POST -COPY W01184                                                    
006100                                                                          
006200 FD  LDC-W01184EX                                                         
006300     RECORDING       F                                                    
006400     BLOCK CONTAINS  0.                                                   
006500                                                                          
006600*01  POST -COPY W01184EX -PRE LDC- -L.                                    
006700                                                                          
006800 FD  SDC-W01184EX                                                         
006900     RECORDING       F                                                    
007000     BLOCK CONTAINS  0.                                                   
007100                                                                          
007200*01  POST -COPY W01184EX -PRE SDC- -L.                                    
007300                                                                          
007400     EJECT                                                                
007500 FD  NDC-W01184EX                                                         
007600     RECORDING       F                                                    
007700     BLOCK CONTAINS  0.                                                   
007800                                                                          
007900*01  POST -COPY W01184EX -PRE NDC- -L.                                    
008000                                                                          
009000 WORKING-STORAGE SECTION.                                                 
009100*    -- CHECKED BY WY2000                                                 
009200 77  IDPGM                       PIC X(8)    VALUE 'W0118B00'.            
009300 77  JA                          PIC X       VALUE 'J'.                   
009400 77  NEJ                         PIC X       VALUE 'N'.                   
009500                                                                          
009600 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
009700     88  END-OF-W01184                       VALUE 'J'.                   
009800                                                                          
009900 77  TAB-DELIMITER               PIC X       VALUE X'05'.                 
010000                                                                          
010100 01  ARBETSAREOR.                                                         
010200     03 IX                       PIC 9(2)    VALUE ZERO.                  
010300                                                                          
010400     EJECT                                                                
010500*      --- VALID IDDC CODES                                               
010600*                                                                         
010700*01    -COPY WWDC99                                                       
010800       EJECT                                                              
010900 01  W01184EX-XDC-AREA.                                                   
011000     03  -COPY W01184EX -PRE- OUT-                                        
011100     EJECT                                                                
011200 01  W01184EX-LDC-AREA.                                                   
011300     03  -COPY W01184EX -PRE LDC-                                         
011400     EJECT                                                                
011500 01  W01184EX-SDC-AREA.                                                   
011600     03  -COPY W01184EX -PRE SDC-                                         
011700     EJECT                                                                
011800 01  W01184EX-NDC-AREA.                                                   
011900     03  -COPY W01184EX -PRE NDC-                                         
012000     EJECT                                                                
012100 01  DYNAMISKA-SUBPROGRAM.                                                
012200*                                                                         
012300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012700     SKIP2                                                                
012800*    --- PARAMETRAR TILL ABEND                                            
012900                                                                          
013000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013200     SKIP2                                                                
013300 01  FELTEXT.                                                             
013400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL POSTSUM                                          
013800*                                                                         
013900*01  -COPY W0005   -PRE  POSTSUM-                                         
014000     EJECT                                                                
014100*    --- STATUS-KOD FRÅN IMS                                              
014200                                                                          
014300 PROCEDURE DIVISION.                                                      
014400 MAIN SECTION.                                                            
014500                                                                          
014600     PERFORM A-INIT                                                       
014700     PERFORM S01-READ-W01184                                              
014800     PERFORM UNTIL END-OF-W01184                                          
014900       PERFORM B-MOVE-W01184                                              
015000       PERFORM S01-READ-W01184                                            
016000     END-PERFORM                                                          
017000                                                                          
018000     PERFORM Z-FINIT                                                      
019000                                                                          
020000     MOVE ZERO TO RETURN-CODE                                             
021000     GOBACK                                                               
022000                                                                          
022100     .                                                                    
022200     EJECT                                                                
022300                                                                          
022400 A-INIT SECTION.                                                          
022500     OPEN INPUT  XDC-W01184                                               
022600     OPEN OUTPUT LDC-W01184EX                                             
022700                 SDC-W01184EX                                             
022800                 NDC-W01184EX                                             
022900                                                                          
023000     INITIALIZE OUT-SLAG-W01184EX                                         
024000                                                                          
025000     .                                                                    
026000     EJECT                                                                
027000                                                                          
027100 B-MOVE-W01184     SECTION.                                               
027200                                                                          
027300     PERFORM BA-MOVE-FIELDS                                               
027400     PERFORM BB-MOVE-TAB-VALUE                                            
032400*                                                                         
032500     MOVE SLAG-IDDC IN SLAG-W01184     TO WS-IDDC                         
032600*                                                                         
032700     IF LDC                                                               
032800       MOVE OUT-SLAG-W01184EX    TO LDC-SLAG-W01184EX                     
032900       PERFORM S10-SKRIV-LDC-W01184EX                                     
033000     ELSE                                                                 
033100       IF SDC                                                             
033200         MOVE OUT-SLAG-W01184EX  TO SDC-SLAG-W01184EX                     
033300         PERFORM S11-SKRIV-SDC-W01184EX                                   
033400       ELSE                                                               
033500         IF NDC                                                           
033600           MOVE OUT-SLAG-W01184EX TO NDC-SLAG-W01184EX                    
033700           PERFORM S12-SKRIV-NDC-W01184EX                                 
033800         END-IF                                                           
033900       END-IF                                                             
034000     END-IF                                                               
034100     .                                                                    
034200     EJECT                                                                
034300                                                                          
034400 BA-MOVE-FIELDS SECTION.                                                  
034500                                                                          
034600     MOVE SLAG-IDARTNR        TO OUT-SLAG-IDARTNR                         
034700     MOVE SLAG-IDDC           TO OUT-SLAG-IDDC                            
034800     MOVE SLAG-ADLAGOMR       TO OUT-SLAG-ADLAGOMR                        
034900     MOVE SLAG-ADGANG         TO OUT-SLAG-ADGANG                          
035000     MOVE SLAG-ADPLATS        TO OUT-SLAG-ADPLATS                         
035100     MOVE SLAG-ADLAGOMR-CD    TO OUT-SLAG-ADLAGOMR-CD                     
035200     MOVE SLAG-BEFT           TO OUT-SLAG-BEFT                            
035300     MOVE SLAG-DAPBPLAN       TO OUT-SLAG-DAPBPLAN                        
035400     MOVE SLAG-DAPUBL         TO OUT-SLAG-DAPUBL                          
035500     MOVE SLAG-DASEASON       TO OUT-SLAG-DASEASON                        
035600     MOVE SLAG-DASPSEA        TO OUT-SLAG-DASPSEA                         
035700     MOVE SLAG-FLCDREL        TO OUT-SLAG-FLCDREL                         
035800     MOVE SLAG-FLFLYG         TO OUT-SLAG-FLFLYG                          
035900     MOVE SLAG-FLJIT          TO OUT-SLAG-FLJIT                           
036000     MOVE SLAG-FLORDSP        TO OUT-SLAG-FLORDSP                         
036100     MOVE SLAG-FLORDSP-EJRO   TO OUT-SLAG-FLORDSP-EJRO                    
036200     MOVE SLAG-FLREFBEO       TO OUT-SLAG-FLREFBEO                        
036300     MOVE SLAG-FLREFLARM      TO OUT-SLAG-FLREFLARM                       
036400     MOVE SLAG-FLSKROT-BEORD  TO OUT-SLAG-FLSKROT-BEORD                   
036500     MOVE SLAG-FLSPBULK       TO OUT-SLAG-FLSPBULK                        
036600     MOVE SLAG-FLWILSON       TO OUT-SLAG-FLWILSON                        
036700     MOVE SLAG-IDANSK         TO OUT-SLAG-IDANSK                          
036800     MOVE SLAG-IDARTNR-EMBQ0  TO OUT-SLAG-IDARTNR-EMBQ0                   
036900     MOVE SLAG-IDARTNR-EMBQ1  TO OUT-SLAG-IDARTNR-EMBQ1                   
037000     MOVE SLAG-IDARTNR-EMBQ2  TO OUT-SLAG-IDARTNR-EMBQ2                   
037100     MOVE SLAG-IDDC-REF       TO OUT-SLAG-IDDC-REF                        
037200     MOVE SLAG-IDINK          TO OUT-SLAG-IDINK                           
037300     MOVE SLAG-IDLANDX2       TO OUT-SLAG-IDLANDX2                        
037400     MOVE SLAG-IDLEVNR        TO OUT-SLAG-IDLEVNR                         
037500     MOVE SLAG-IDPERSON-BUY   TO OUT-SLAG-IDPERSON-BUY                    
037600     MOVE SLAG-IDPLANGR-AG    TO OUT-SLAG-IDPLANGR-AG                     
037700     MOVE SLAG-IDPSN-DC       TO OUT-SLAG-IDPSN-DC                        
037800     MOVE SLAG-IDREFTAB       TO OUT-SLAG-IDREFTAB                        
037900     MOVE SLAG-IDUSER-SPKVAL  TO OUT-SLAG-IDUSER-SPKVAL                   
038000     MOVE SLAG-KDARTURS       TO OUT-SLAG-KDARTURS                        
038100     MOVE SLAG-KDAVT          TO OUT-SLAG-KDAVT                           
038200     MOVE SLAG-KDLEVPLF       TO OUT-SLAG-KDLEVPLF                        
038300     MOVE SLAG-KDLEVSP        TO OUT-SLAG-KDLEVSP                         
038400     MOVE SLAG-KDLPSP         TO OUT-SLAG-KDLPSP                          
038500     MOVE SLAG-KDOPPLAN       TO OUT-SLAG-KDOPPLAN                        
038600     MOVE SLAG-KDREFSTA       TO OUT-SLAG-KDREFSTA                        
038700     MOVE SLAG-KVAKS-PAV      TO OUT-SLAG-KVAKS-PAV                       
038800     MOVE SLAG-KVAKS-SDC      TO OUT-SLAG-KVAKS-SDC                       
038900     MOVE SLAG-KVPBREOI       TO OUT-SLAG-KVPBREOI                        
039000     MOVE SLAG-KVBEART        TO OUT-SLAG-KVBEART                         
039100     MOVE SLAG-KVDAGAR-CDBEH  TO OUT-SLAG-KVDAGAR-CDBEH                   
039200     MOVE SLAG-KVDAGAR-FFH    TO OUT-SLAG-KVDAGAR-FFH                     
039300     MOVE SLAG-KVDAGAR-INLEV  TO OUT-SLAG-KVDAGAR-INLEV                   
039400     MOVE SLAG-KVDAGAR-MANLT  TO OUT-SLAG-KVDAGAR-MANLT                   
039500     MOVE SLAG-KVEFRS         TO OUT-SLAG-KVEFRS                          
039600     MOVE SLAG-KVEOQ          TO OUT-SLAG-KVEOQ                           
039700     MOVE SLAG-KVINVS         TO OUT-SLAG-KVINVS                          
039800     MOVE SLAG-KVLS           TO OUT-SLAG-KVLS                            
039900     MOVE SLAG-KVOKS-BULK     TO OUT-SLAG-KVOKS-BULK                      
040000     MOVE SLAG-KVOKS-DAG      TO OUT-SLAG-KVOKS-DAG                       
040100     MOVE SLAG-KVPALL         TO OUT-SLAG-KVPALL                          
040200     MOVE SLAG-KVPB-PLAN      TO OUT-SLAG-KVPB-PLAN                       
040300     MOVE SLAG-KVPB-REF       TO OUT-SLAG-KVPB-REF                        
040400     MOVE SLAG-KVPB-TREND     TO OUT-SLAG-KVPB-TREND                      
040500     MOVE SLAG-KVREFBER       TO OUT-SLAG-KVREFBER                        
040600     MOVE SLAG-KVREFOVL       TO OUT-SLAG-KVREFOVL                        
040700     MOVE SLAG-KVREFPKT       TO OUT-SLAG-KVREFPKT                        
040800     MOVE SLAG-KVRESS         TO OUT-SLAG-KVRESS                          
040900     MOVE SLAG-KVRETUR-BEORD  TO OUT-SLAG-KVRETUR-BEORD                   
041000     MOVE SLAG-KVROS-BULK     TO OUT-SLAG-KVROS-BULK                      
041100     MOVE SLAG-KVROS-DAG      TO OUT-SLAG-KVROS-DAG                       
041200     MOVE SLAG-KVSKROT        TO OUT-SLAG-KVSKROT                         
041300     MOVE SLAG-KVSLAGER       TO OUT-SLAG-KVSLAGER                        
041400     MOVE SLAG-KVSLUTKP       TO OUT-SLAG-KVSLUTKP                        
041500     MOVE SLAG-KVSPANT        TO OUT-SLAG-KVSPANT                         
041600     MOVE SLAG-KVSPARR-KVAL   TO OUT-SLAG-KVSPARR-KVAL                    
041700     MOVE SLAG-KVULOAD        TO OUT-SLAG-KVULOAD                         
041800     MOVE SLAG-KVUTRS         TO OUT-SLAG-KVUTRS                          
041900     MOVE SLAG-KVVECKOR-FT    TO OUT-SLAG-KVVECKOR-FT                     
042000     MOVE SLAG-KVVECKOR-LT    TO OUT-SLAG-KVVECKOR-LT                     
042100     MOVE SLAG-KVVECKOR-TREND TO OUT-SLAG-KVVECKOR-TREND                  
042200     MOVE SLAG-PRARTSJK       TO OUT-SLAG-PRARTSJK                        
042300     MOVE SLAG-PRAVCOST       TO OUT-SLAG-PRAVCOST                        
042400*REASON ARRAY                                                             
042500     MOVE SLAG-RESEASON IN SLAG-W01184(1)  TO OUT-SLAG-RESEASON1          
042600     MOVE SLAG-RESEASON IN SLAG-W01184(2)  TO OUT-SLAG-RESEASON2          
042700     MOVE SLAG-RESEASON IN SLAG-W01184(3)  TO OUT-SLAG-RESEASON3          
042800     MOVE SLAG-RESEASON IN SLAG-W01184(4)  TO OUT-SLAG-RESEASON4          
042900     MOVE SLAG-RESEASON IN SLAG-W01184(5)  TO OUT-SLAG-RESEASON5          
043000     MOVE SLAG-RESEASON IN SLAG-W01184(6)  TO OUT-SLAG-RESEASON6          
043100     MOVE SLAG-RESEASON IN SLAG-W01184(7)  TO OUT-SLAG-RESEASON7          
043200     MOVE SLAG-RESEASON IN SLAG-W01184(8)  TO OUT-SLAG-RESEASON8          
043300     MOVE SLAG-RESEASON IN SLAG-W01184(9)  TO OUT-SLAG-RESEASON9          
043400     MOVE SLAG-RESEASON IN SLAG-W01184(10) TO OUT-SLAG-RESEASON10         
043500     MOVE SLAG-RESEASON IN SLAG-W01184(11) TO OUT-SLAG-RESEASON11         
043600     MOVE SLAG-RESEASON IN SLAG-W01184(12) TO OUT-SLAG-RESEASON12         
043700*RESEASON-PLAN                                                            
043800     MOVE SLAG-RESEASON-PLAN IN SLAG-W01184(1)                            
043900                                        TO OUT-SLAG-RESEASON-PLAN1        
044000     MOVE SLAG-RESEASON-PLAN IN SLAG-W01184(2)                            
044100                                        TO OUT-SLAG-RESEASON-PLAN2        
044200     MOVE SLAG-RESEASON-PLAN IN SLAG-W01184(3)                            
044300                                        TO OUT-SLAG-RESEASON-PLAN3        
044400     MOVE SLAG-RESEASON-PLAN IN SLAG-W01184(4)                            
044500                                        TO OUT-SLAG-RESEASON-PLAN4        
044600     MOVE SLAG-RESEASON-PLAN IN SLAG-W01184(5)                            
044700                                        TO OUT-SLAG-RESEASON-PLAN5        
044800     MOVE SLAG-RESEASON-PLAN IN SLAG-W01184(6)                            
044900                                        TO OUT-SLAG-RESEASON-PLAN6        
045000     MOVE SLAG-RESEASON-PLAN IN SLAG-W01184(7)                            
045100                                        TO OUT-SLAG-RESEASON-PLAN7        
045200     MOVE SLAG-RESEASON-PLAN IN SLAG-W01184(8)                            
045300                                        TO OUT-SLAG-RESEASON-PLAN8        
045400     MOVE SLAG-RESEASON-PLAN IN SLAG-W01184(9)                            
045500                                        TO OUT-SLAG-RESEASON-PLAN9        
045600     MOVE SLAG-RESEASON-PLAN IN SLAG-W01184(10)                           
045700                                       TO OUT-SLAG-RESEASON-PLAN10        
045800     MOVE SLAG-RESEASON-PLAN IN SLAG-W01184(11)                           
045900                                       TO OUT-SLAG-RESEASON-PLAN11        
046000     MOVE SLAG-RESEASON-PLAN IN SLAG-W01184(12)                           
046100                                       TO OUT-SLAG-RESEASON-PLAN12        
046200                                                                          
046300     MOVE SLAG-RETREND       TO OUT-SLAG-RETREND                          
046400     MOVE SLAG-TEKVAL        TO OUT-SLAG-TEKVAL                           
046500     MOVE SLAG-TIAVCOST      TO OUT-SLAG-TIAVCOST                         
046600     MOVE SLAG-TIDATUM-TREND TO OUT-SLAG-TIDATUM-TREND                    
046700     MOVE SLAG-TIINVDAT      TO OUT-SLAG-TIINVDAT                         
046800*TIINVDAG                                                                 
046900     MOVE SLAG-TILEVDAG IN SLAG-W01184(1) TO                              
046910                                      OUT-SLAG-TIINVDAG1                  
047000     MOVE SLAG-TILEVDAG IN SLAG-W01184(2) TO                              
047010                                      OUT-SLAG-TIINVDAG2                  
047100     MOVE SLAG-TILEVDAG IN SLAG-W01184(3) TO                              
047110                                      OUT-SLAG-TIINVDAG3                  
047200     MOVE SLAG-TILEVDAG IN SLAG-W01184(4) TO                              
047210                                      OUT-SLAG-TIINVDAG4                  
047300     MOVE SLAG-TILEVDAG IN SLAG-W01184(5) TO                              
047310                                      OUT-SLAG-TIINVDAG5                  
047400                                                                          
047500     MOVE SLAG-TILPSP           TO OUT-SLAG-TILPSP                        
047600     MOVE SLAG-TIMANSEA         TO OUT-SLAG-TIMANSEA                      
047700     MOVE SLAG-TIOMSPEC         TO OUT-SLAG-TIOMSPEC                      
047800     MOVE SLAG-TIORDREG         TO OUT-SLAG-TIORDREG                      
047900     MOVE SLAG-TIPBREOI         TO OUT-SLAG-TIPBREOI                      
048000     MOVE SLAG-TIREFMPB         TO OUT-SLAG-TIREFMPB                      
048100     MOVE SLAG-TIREFPAF         TO OUT-SLAG-TIREFPAF                      
048200     MOVE SLAG-TIREFPKT         TO OUT-SLAG-TIREFPKT                      
048300     MOVE SLAG-TIREFSTA         TO OUT-SLAG-TIREFSTA                      
048400     MOVE SLAG-TIREFSTO         TO OUT-SLAG-TIREFSTO                      
048500     MOVE SLAG-TIREFSTO-LOC     TO OUT-SLAG-TIREFSTO-LOC                  
048600     MOVE SLAG-TIRETUR-BEORD    TO OUT-SLAG-TIRETUR-BEORD                 
048700     MOVE SLAG-TISKROT          TO OUT-SLAG-TISKROT                       
048800     MOVE SLAG-TISKROT-BEORD    TO OUT-SLAG-TISKROT-BEORD                 
048900     MOVE SLAG-TISLUTKP         TO OUT-SLAG-TISLUTKP                      
049000     MOVE SLAG-TISPARR-KVAL     TO OUT-SLAG-TISPARR-KVAL                  
049100     MOVE SLAG-VKART            TO OUT-SLAG-VKART                         
049200     MOVE SLAG-VLARTNTO         TO OUT-SLAG-VLARTNTO                      
049300     MOVE SLAG-IDLEVNR-SHIP     TO OUT-SLAG-IDLEVNR-SHIP                  
049400     MOVE SLAG-PRMATRL          TO OUT-SLAG-PRMATRL                       
049500     MOVE SLAG-TIERSDAT-VIPS    TO OUT-SLAG-TIERSDAT-VIPS                 
049600     MOVE SLAG-TIMANSEC         TO OUT-SLAG-TIMANSEC                      
049700     MOVE SLAG-KDMATRPR         TO OUT-SLAG-KDMATRPR                      
049800     MOVE SLAG-KVPB-JUST1       TO OUT-SLAG-KVPB-JUST1                    
049900     MOVE SLAG-TIPBJUST-1       TO OUT-SLAG-TIPBJUST-1                    
050000     MOVE SLAG-KVPB-JUST2       TO OUT-SLAG-KVPB-JUST2                    
050100     MOVE SLAG-TIPBJUST-2       TO OUT-SLAG-TIPBJUST-2                    
050200     MOVE SLAG-IDLEVNR-FRAM     TO OUT-SLAG-IDLEVNR-FRAM                  
050300     MOVE SLAG-IDLEVNR-SHIP-FRAM TO OUT-SLAG-IDLEVNR-SHIP-FRAM            
050400     MOVE SLAG-KVSLAGER         TO OUT-SLAG-KVSLAGER                      
050500     MOVE SLAG-TILEVDAT         TO OUT-SLAG-TILEVDAT                      
050600     MOVE SLAG-TIMANLED         TO OUT-SLAG-TIMANLED                      
050700     MOVE SLAG-TISTODAT-LARM    TO OUT-SLAG-TISTODAT-LARM                 
050800     MOVE SLAG-FLREFILL         TO OUT-SLAG-FLREFILL                      
050900     MOVE SLAG-FLREFNYO         TO OUT-SLAG-FLREFNYO                      
051000     MOVE SLAG-TISKROT-AUTO     TO OUT-SLAG-TISKROT-AUTO                  
051100     MOVE SLAG-TIUPPDAT-EMB     TO OUT-SLAG-TIUPPDAT-EMB                  
051200     MOVE SLAG-FLBUYUPD         TO OUT-SLAG-FLBUYUPD                      
051300     MOVE SLAG-FLTABUPD         TO OUT-SLAG-FLTABUPD                      
051400     MOVE SLAG-REPPFAKT         TO OUT-SLAG-REPPFAKT                      
051500     MOVE SLAG-FLPB-FLYTT       TO OUT-SLAG-FLPB-FLYTT                    
051600     MOVE SLAG-FLLARM-BUF       TO OUT-SLAG-FLLARM-BUF                    
051700     .                                                                    
051800     EJECT                                                                
051900                                                                          
052000 BB-MOVE-TAB-VALUE SECTION.                                               
052100     MOVE TAB-DELIMITER  TO   OUT-SLAG-FILL1                              
052200                              OUT-SLAG-FILL2                              
052300                              OUT-SLAG-FILL3                              
052400                              OUT-SLAG-FILL4                              
052500                              OUT-SLAG-FILL5                              
052600                              OUT-SLAG-FILL6                              
052700                              OUT-SLAG-FILL7                              
052800                              OUT-SLAG-FILL8                              
052900                              OUT-SLAG-FILL9                              
053000                              OUT-SLAG-FILL10                             
053100                              OUT-SLAG-FILL11                             
053200                              OUT-SLAG-FILL12                             
053300                              OUT-SLAG-FILL13                             
053400                              OUT-SLAG-FILL14                             
053500                              OUT-SLAG-FILL15                             
053600                              OUT-SLAG-FILL16                             
053700                              OUT-SLAG-FILL17                             
053800                              OUT-SLAG-FILL18                             
053900                              OUT-SLAG-FILL19                             
054000                              OUT-SLAG-FILL20                             
054100                              OUT-SLAG-FILL21                             
054200                              OUT-SLAG-FILL22                             
054300                              OUT-SLAG-FILL23                             
054400                              OUT-SLAG-FILL24                             
054500                              OUT-SLAG-FILL25                             
054600                              OUT-SLAG-FILL26                             
054700                              OUT-SLAG-FILL27                             
054800                              OUT-SLAG-FILL28                             
054900                              OUT-SLAG-FILL29                             
055000                              OUT-SLAG-FILL30                             
055100                              OUT-SLAG-FILL31                             
055200                              OUT-SLAG-FILL32                             
055300                              OUT-SLAG-FILL33                             
055400                              OUT-SLAG-FILL34                             
055500                              OUT-SLAG-FILL35                             
055600                              OUT-SLAG-FILL36                             
055700                              OUT-SLAG-FILL37                             
055800                              OUT-SLAG-FILL38                             
055900                              OUT-SLAG-FILL39                             
056000                              OUT-SLAG-FILL40                             
056100                              OUT-SLAG-FILL41                             
056200                              OUT-SLAG-FILL42                             
056300                              OUT-SLAG-FILL43                             
056400                              OUT-SLAG-FILL44                             
056500                              OUT-SLAG-FILL45                             
056600                              OUT-SLAG-FILL46                             
056700                              OUT-SLAG-FILL47                             
056800                              OUT-SLAG-FILL48                             
056900                              OUT-SLAG-FILL49                             
057000                              OUT-SLAG-FILL50                             
057100                              OUT-SLAG-FILL51                             
057200                              OUT-SLAG-FILL52                             
057300                              OUT-SLAG-FILL53                             
057400                              OUT-SLAG-FILL54                             
057500                              OUT-SLAG-FILL55                             
057600                              OUT-SLAG-FILL56                             
057700                              OUT-SLAG-FILL57                             
057800                              OUT-SLAG-FILL58                             
057900                              OUT-SLAG-FILL59                             
058000                              OUT-SLAG-FILL60                             
058100                              OUT-SLAG-FILL61                             
058200                              OUT-SLAG-FILL62                             
058300                              OUT-SLAG-FILL63                             
058400                              OUT-SLAG-FILL64                             
058500                              OUT-SLAG-FILL65                             
058600                              OUT-SLAG-FILL66                             
058700                              OUT-SLAG-FILL67                             
058800                              OUT-SLAG-FILL68                             
058900                              OUT-SLAG-FILL69                             
059000                              OUT-SLAG-FILL70                             
059100                              OUT-SLAG-FILL71                             
059200                              OUT-SLAG-FILL72                             
059300                              OUT-SLAG-FILL73                             
059400                              OUT-SLAG-FILL74                             
059500                              OUT-SLAG-FILL75                             
059600                              OUT-SLAG-FILL76                             
059700                              OUT-SLAG-FILL77                             
059800                              OUT-SLAG-FILL78                             
059900                              OUT-SLAG-FILL79                             
060000                              OUT-SLAG-FILL80                             
060100                              OUT-SLAG-FILL81                             
060200                              OUT-SLAG-FILL82                             
060300                              OUT-SLAG-FILL83                             
060400                              OUT-SLAG-FILL84                             
060500                              OUT-SLAG-FILL85                             
060600                              OUT-SLAG-FILL86                             
060700                              OUT-SLAG-FILL87                             
060800                              OUT-SLAG-FILL88                             
060900                              OUT-SLAG-FILL89                             
061000                              OUT-SLAG-FILL90                             
061100                              OUT-SLAG-FILL91                             
061200                              OUT-SLAG-FILL92                             
061300                              OUT-SLAG-FILL93                             
061400                              OUT-SLAG-FILL94                             
061500                              OUT-SLAG-FILL95                             
061600                              OUT-SLAG-FILL96                             
061700                              OUT-SLAG-FILL97                             
061800                              OUT-SLAG-FILL98                             
061900                              OUT-SLAG-FILL99                             
062000                              OUT-SLAG-FILL100                            
062100                              OUT-SLAG-FILL101                            
062200                              OUT-SLAG-FILL102                            
062300                              OUT-SLAG-FILL103                            
062400                              OUT-SLAG-FILL104                            
062500                              OUT-SLAG-FILL105                            
062600                              OUT-SLAG-FILL106                            
062700                              OUT-SLAG-FILL107                            
062800                              OUT-SLAG-FILL108                            
062900                              OUT-SLAG-FILL109                            
063000                              OUT-SLAG-FILL110                            
063100                              OUT-SLAG-FILL111                            
063200                              OUT-SLAG-FILL112                            
063300                              OUT-SLAG-FILL113                            
063400                              OUT-SLAG-FILL114                            
063500                              OUT-SLAG-FILL115                            
063600                              OUT-SLAG-FILL116                            
063700                              OUT-SLAG-FILL117                            
063800                              OUT-SLAG-FILL118                            
063900                              OUT-SLAG-FILL119                            
064000                              OUT-SLAG-FILL120                            
064100                              OUT-SLAG-FILL121                            
064200                              OUT-SLAG-FILL122                            
064300                              OUT-SLAG-FILL123                            
064400                              OUT-SLAG-FILL124                            
064500                              OUT-SLAG-FILL125                            
064600                              OUT-SLAG-FILL126                            
064700                              OUT-SLAG-FILL127                            
064800                              OUT-SLAG-FILL128                            
064900                              OUT-SLAG-FILL129                            
065000                              OUT-SLAG-FILL130                            
065100                              OUT-SLAG-FILL131                            
065200                              OUT-SLAG-FILL132                            
065300                              OUT-SLAG-FILL133                            
065400                              OUT-SLAG-FILL134                            
065500                              OUT-SLAG-FILL135                            
065600                              OUT-SLAG-FILL136                            
065700                              OUT-SLAG-FILL137                            
065800                              OUT-SLAG-FILL138                            
065900                              OUT-SLAG-FILL139                            
066000                              OUT-SLAG-FILL140                            
066100                              OUT-SLAG-FILL141                            
066200                              OUT-SLAG-FILL142                            
066300                              OUT-SLAG-FILL143                            
066400                              OUT-SLAG-FILL144                            
066500                              OUT-SLAG-FILL145                            
066600                              OUT-SLAG-FILL146                            
066700                              OUT-SLAG-FILL147                            
066800                              OUT-SLAG-FILL148                            
066900                              OUT-SLAG-FILL149                            
067000                              OUT-SLAG-FILL150                            
067100                              OUT-SLAG-FILL151                            
067200                              OUT-SLAG-FILL152                            
067210                              OUT-SLAG-FILL153                            
067300     .                                                                    
067400     EJECT                                                                
067500                                                                          
067600 Z-FINIT SECTION.                                                         
067700     CLOSE XDC-W01184                                                     
067800           LDC-W01184EX                                                   
067900           SDC-W01184EX                                                   
068000           NDC-W01184EX                                                   
068100     MOVE 'S' TO POSTSUM-OPKOD                                            
068200     CALL POSTSUM USING POSTSUM-PARM                                      
068300     .                                                                    
068400     EJECT                                                                
068500                                                                          
068600 S01-READ-W01184 SECTION.                                                 
068700     SKIP2                                                                
068800     READ XDC-W01184 RECORD INTO W01184EX-XDC-AREA                        
068900     AT END                                                               
069000        MOVE JA              TO W01184-EOF-SW                             
069100     END-READ                                                             
069200     .                                                                    
069300     EJECT                                                                
069400                                                                          
069500 S10-SKRIV-LDC-W01184EX SECTION.                                          
069600     WRITE LDC-POST FROM W01184EX-LDC-AREA                                
069700                                                                          
069800     MOVE 'W0118B' TO POSTSUM-FDNAMN                                      
069900     MOVE 'W0118BD1' TO POSTSUM-DDNAMN2                                   
070000     CALL POSTSUM USING POSTSUM-PARM                                      
070100     .                                                                    
070200     EJECT                                                                
070300 S11-SKRIV-SDC-W01184EX SECTION.                                          
070400     WRITE SDC-POST FROM W01184EX-SDC-AREA                                
070500                                                                          
070600     MOVE 'W0118B' TO POSTSUM-FDNAMN                                      
070700     MOVE 'W0118BD2' TO POSTSUM-DDNAMN2                                   
070800     CALL POSTSUM USING POSTSUM-PARM                                      
070900     .                                                                    
071000     EJECT                                                                
071100 S12-SKRIV-NDC-W01184EX SECTION.                                          
071200     WRITE NDC-POST FROM W01184EX-NDC-AREA                                
071300                                                                          
071400     MOVE 'W0118B' TO POSTSUM-FDNAMN                                      
071500     MOVE 'W0118BD3' TO POSTSUM-DDNAMN2                                   
071600     CALL POSTSUM USING POSTSUM-PARM                                      
071700     .                                                                    
071800     EJECT                                                                
072000                                                                          
080000                                                                          
