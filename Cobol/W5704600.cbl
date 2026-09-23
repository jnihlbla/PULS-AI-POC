000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5704600.                                                
000301 AUTHOR.         UMESH JAIN.                                              
000402 DATE-WRITTEN.   MAY 2012.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
002601*                                                                         
002701*       -PROGRAMMET LÄSER      WDH5                                       
003301*                                                                         
003401*    ABENDKODER:                                                          
003501*        U0016 -  . . . .                                                 
003601*        U1000 -  . . . .                                                 
003701*                                                                         
003801                                                                          
003901 ENVIRONMENT DIVISION.                                                    
004001                                                                          
004101 INPUT-OUTPUT SECTION.                                                    
004201                                                                          
004301 FILE-CONTROL.                                                            
004402*          --- INPUT FROM W57045                                          
004501     SELECT W57045                     ASSIGN TO W57046D1.                
004601                                                                          
004701*          --- OUTPUT WITH CHINESE CURRENCY                               
004801     SELECT W57046                     ASSIGN TO W57046D2.                
004901                                                                          
007301     EJECT                                                                
007401                                                                          
007501 DATA DIVISION.                                                           
007601                                                                          
007701 FILE SECTION.                                                            
007801 FD  W57045                                                               
007901     RECORDING       F                                                    
008001     BLOCK CONTAINS  0.                                                   
008101 01  IN-POST.                                                             
008201*    03  -COPY W57045    -PRE  IN-  -L.                                   
010201                                                                          
010301 FD  W57046                                                               
010401     RECORDING       F                                                    
010501     BLOCK CONTAINS  0.                                                   
010705*01  POST   -COPY W57046 -PRE  UT-  -L.                                   
010801                                                                          
013901 WORKING-STORAGE SECTION.                                                 
014101 77  IDPGM                        PIC X(8)    VALUE 'W5704600'.           
014201 77  JA                           PIC X       VALUE 'J'.                  
014301 77  NEJ                          PIC X       VALUE 'N'.                  
014405 77  FELTEXT                      PIC X(80).                              
014505 77  WS-ACTUAL-DATE               PIC S9(16) COMP-3 VALUE ZERO.           
014605 77  WS-ACTUAL-DATE-X             PIC X(16)   VALUE ZERO.                 
014705 77  W57045-EOF-SW                PIC X       VALUE 'N'.                  
014805     88  END-OF-W57045                        VALUE 'J'.                  
014806 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
014807 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
014808 77  WS-SAVE-KDVALISO             PIC X(3)    VALUE SPACE.                
014809 77  WS-SAVE-MONTH                PIC 9(4)    VALUE ZERO.                 
014810 77  W-PRKURS                     PIC S9(6)V9(5) VALUE +0                 
014820                                                   COMP-3.                
014905                                                                          
027400 01  DYNAMISKA-SUBPROGRAM.                                                
027500     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
027600     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
027700     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
027900     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
028000     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
028001                                                                          
029800*    --- PARAMETRAR TILL POSTSUM                                          
029900*01  -COPY W0005   -PRE  POSTSUM-                                         
030000     EJECT                                                                
030100*01  -COPY W510CURR                                                       
030101     EJECT                                                                
030102                                                                          
030204 01  IN-AREA-START               PIC X(24)   VALUE                        
030304                                 'IN-AREA-START  '.                       
030404                                                                          
030504*01  AREA -COPY W57045     -PRE IN-                                       
030604     EJECT                                                                
033600                                                                          
033704 01  UT-AREA-START               PIC X(24)   VALUE                        
033804                                 'UT-AREA-START  '.                       
033904                                                                          
034004*01  AREA -COPY W57046     -PRE UT-                                       
034104     EJECT                                                                
034204                                                                          
045500 LINKAGE SECTION.                                                         
045601*01  -COPY W0008  -PRE WDG2-                                              
045700     05  FILLER                  PIC X.                                   
045800                                                                          
047400     EJECT                                                                
047500                                                                          
047604 PROCEDURE DIVISION  USING WDG2-PCB.                                      
047701                                                                          
047800 MAIN SECTION.                                                            
047904     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
048100                                                                          
048200     PERFORM A-INIT                                                       
048300                                                                          
048401     PERFORM S01-READ-W57045                                              
048501     PERFORM UNTIL END-OF-W57045                                          
048601       PERFORM B-CONVERT-CURRENCY-TO-CNY                                  
049801       PERFORM S01-READ-W57045                                            
049900     END-PERFORM                                                          
050000                                                                          
050100     PERFORM Z-FINIT                                                      
050300     MOVE ZERO TO RETURN-CODE                                             
050400     GOBACK                                                               
050500     .                                                                    
050600     EJECT                                                                
050700                                                                          
050800 A-INIT SECTION.                                                          
050901     OPEN INPUT  W57045                                                   
051101     OPEN OUTPUT W57046                                                   
056500     .                                                                    
056600     EJECT                                                                
056700                                                                          
056801 B-CONVERT-CURRENCY-TO-CNY SECTION.                                       
056802     PERFORM BA-GET-EXCHRATE                                              
056803     COMPUTE IN-PRARTNTO   ROUNDED =                                      
056804            (IN-PRARTNTO * IN-KVAVIS) / W-PRKURS                          
057342     MOVE IN-IDARTNR              TO UT-IDARTNR                           
057343     MOVE IN-IDDC                 TO UT-IDDC                              
057344     MOVE IN-DAINLEV              TO UT-DAINLEV                           
057345     MOVE IN-KVAVIS               TO UT-KVAVIS                            
057346     MOVE IN-PRARTNTO             TO UT-PRARTNTO                          
057347     MOVE IN-KDVALISO             TO UT-KDVALISO                          
057348     MOVE IN-KDTRADP              TO UT-KDTRADP                           
057349     PERFORM S02-WRITE-W57046                                             
058601     .                                                                    
059001     EJECT                                                                
059002 BA-GET-EXCHRATE SECTION.                                                 
059003                                                                          
059004     COMPUTE WS-ACTUAL-DATE    = 9999999999999999                         
059005                                  - IN-DAINLEV                            
059006     MOVE WS-ACTUAL-DATE             TO WS-ACTUAL-DATE-X                  
059007     MOVE WS-ACTUAL-DATE-X(3:4)      TO W-DATE-AAMM                       
059008                                                                          
059009     IF IN-KDVALISO = WS-SAVE-KDVALISO                                    
059010     AND W-DATE-AAMM = WS-SAVE-MONTH                                      
059020       CONTINUE                                                           
059030     ELSE                                                                 
059040       MOVE IN-KDVALISO           TO WS-SAVE-KDVALISO                     
059050                                                                          
059060       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
059070                                     WS-SAVE-MONTH                        
059080       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
059090       MOVE IN-KDVALISO           TO CURR-KDVALISO-ROW                    
059100       MOVE 'M'                   TO CURR-KDVALTYP                        
059200       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
059300       IF CURR-KDSVAR = ' '                                               
059400         MOVE CURR-PRKURS-NEW     TO W-PRKURS                             
059500       ELSE                                                               
059600         MOVE 1                   TO W-PRKURS                             
059700       END-IF                                                             
059800                                                                          
059900     END-IF                                                               
060000     .                                                                    
060001     EJECT                                                                
060002                                                                          
060003 Z-FINIT SECTION.                                                         
060004     CLOSE W57045                                                         
060005     CLOSE W57046                                                         
060006     MOVE 'S' TO POSTSUM-OPKOD                                            
060007     CALL POSTSUM USING POSTSUM-PARM                                      
060008     .                                                                    
060009     EJECT                                                                
412900                                                                          
413001 S01-READ-W57045  SECTION.                                                
413101     READ W57045 INTO IN-AREA                                             
413201     AT END                                                               
413401        SET END-OF-W57045 TO TRUE                                         
413501                                                                          
413601     NOT AT END                                                           
413701        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
413801        MOVE 'W57045'     TO POSTSUM-FDNAMN                               
413901        MOVE 'W57046D1'   TO POSTSUM-DDNAMN2                              
414001        CALL POSTSUM USING POSTSUM-PARM                                   
414101     END-READ                                                             
414201     .                                                                    
414301                                                                          
414401 S02-WRITE-W57046 SECTION.                                                
415504     WRITE UT-POST              FROM UT-AREA                              
415601                                                                          
415602     MOVE 'UT'                  TO POSTSUM-TRANSTYP                       
415801     MOVE 'W57046'              TO POSTSUM-FDNAMN                         
415901     MOVE 'W57046D2'            TO POSTSUM-DDNAMN2                        
416001     CALL POSTSUM USING POSTSUM-PARM                                      
416101     .                                                                    
416201                                                                          
