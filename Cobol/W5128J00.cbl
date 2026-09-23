000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5128J00.                                                
000302 AUTHOR.         BARSHARANI BISHOYE.                                      
000401 DATE-WRITTEN.   03/01/20.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNCTION:                                                            
000901*        EXTRACT R31 AND 310 FROM WDL6                                    
001002*                                                                         
001102*        THE PROGRAM READS     WDL6                                       
001202                                                                          
001301 ENVIRONMENT DIVISION.                                                    
001402     SKIP2                                                                
001501 INPUT-OUTPUT SECTION.                                                    
001602                                                                          
001701 FILE-CONTROL.                                                            
001801     SKIP2                                                                
001902*          --- EXTRACT R31 AND 310 FROM WDL6                              
002001     SELECT W5128J    ASSIGN     TO W5128JD1.                             
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP2                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W5128J                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003201*01  RECORD -COPY W5128J -PRE  UT-  -L.                                   
003300     EJECT                                                                
003402                                                                          
005301 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005501 77  IDPGM                       PIC X(8)    VALUE 'W5128J00'.            
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
006002 77  WS-DATE-DISPLAY             PIC 9(16)   VALUE ZERO.                  
006003     EJECT                                                                
006004 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006005 01  FILLER REDEFINES TODAYS-DATE.                                        
006006     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006007     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006008     03  TODAYS-DATE-DAY         PIC 9(2).                                
006009     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006210     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600                                                                          
006610*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006620                                                                          
006630 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006640 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006650 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006660     SKIP2                                                                
006700 01  ERROR-TEXT.                                                          
006800     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007000     EJECT                                                                
007100*01  -COPY W0005   -PRE  POSTSUM-                                         
007200     EJECT                                                                
007301*01  -COPY WWDC99                                                         
007401     EJECT                                                                
007700 01  UT-AREA-START               PIC X(24)   VALUE                        
007800                                 'UT-AREA-START  '.                       
007901*01  AREA -COPY W5128J     -PRE UT-                                       
008000     EJECT                                                                
009700*                                                                         
009800*    --- STATUS-KOD FRÅN IMS                                              
009902 01  STATUS-WS                   PIC XX.                                  
010002     88  SEGMENT-FOUND                       VALUE '  '.                  
010102     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010202     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010302     88  SEGMENT-END-OF-DB                   VALUE 'GB'.                  
010402     SKIP2                                                                
010502 01  GOOD-STATUSCODES.                                                    
010602     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010702     SKIP3                                                                
010900 01  SSA1                        PIC X(160).                              
011000     EJECT                                                                
011101 01  SSA2                        PIC X(160).                              
011201     EJECT                                                                
011300*    --- IMS FUNKTIONSKODER                                               
011400*01  -COPY W0003                                                          
011500     EJECT                                                                
012200*    ---  DLI INPUT-OUTPUT AREA                                           
012300 01  FILLER         PIC X(16)    VALUE 'DLI-IO-WDL601'.                   
012400 01  DLI-IO-AREA.                                                         
012500     03 IO-AREA     PIC X(1600)   VALUE SPACE.                            
012510     SKIP3                                                                
012600     03 WDL601     REDEFINES IO-AREA.                                     
012700*       05 -COPY WDL601                                                   
012800     SKIP3                                                                
012900     03 WDL611     REDEFINES IO-AREA.                                     
013000*       05 -COPY WDL611                                                   
013101     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000                                                                          
014100*01  -COPY W0008  -PRE WDL6-                                              
014200     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800 PROCEDURE DIVISION    USING WDL6-PCB.                                    
014900 MAIN SECTION.                                                            
015000     ENTRY 'DLITCBL'   USING WDL6-PCB.                                    
015100                                                                          
015200     PERFORM A-INIT                                                       
015300                                                                          
015402     PERFORM IMS-GN-WDL6                                                  
015502     PERFORM UNTIL SEGMENT-END-OF-DB                                      
015602       EVALUATE WDL6-SEG-NAME-FB                                          
015702         WHEN 'WDL601'                                                    
015802          MOVE ART-IDARTNR      TO UT-IDARTNR                             
015902                                                                          
016002         WHEN 'WDL611'                                                    
017002          IF INL-IDPTYP = 'R31' OR '310'                                  
018002            PERFORM B-PROCESS                                             
019002          END-IF                                                          
020002       END-EVALUATE                                                       
030002       PERFORM IMS-GN-WDL6                                                
040002     END-PERFORM                                                          
050002                                                                          
060002     PERFORM Z-FINIT                                                      
070002                                                                          
080002     MOVE ZERO TO RETURN-CODE                                             
090002     GOBACK                                                               
100002     .                                                                    
110002     EJECT                                                                
120002 A-INIT SECTION.                                                          
130002                                                                          
140002     OPEN OUTPUT W5128J                                                   
150002                                                                          
160002     ACCEPT TODAYS-DATE  FROM DATE                                        
170002     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
180002     .                                                                    
190002     EJECT                                                                
200002 B-PROCESS SECTION.                                                       
210002                                                                          
220002     MOVE INL-IDDC TO WS-IDDC                                             
230002     IF (INL-IDDISTR <= 9999) AND                                         
240002        NOT (NDC-NA OR XDC-NON-VCC-OWNED)                                 
240003       MOVE INL-IDPTYP   TO UT-IDPTYP                                     
240005       MOVE INL-IDDC     TO UT-IDDC                                       
250002       MOVE INL-IDLEVNR  TO UT-IDLEVNR                                    
250003       MOVE INL-IDDISTR  TO UT-IDDISTR                                    
250004       MOVE INL-IDKUNDNR TO UT-IDKUNDNR                                   
250005       MOVE INL-IDORDNR5 TO UT-IDORDNR5                                   
250006       MOVE INL-IDFAKT   TO UT-IDFAKT                                     
250007       MOVE INL-IDLOPNRM TO UT-IDLOPNRM                                   
250008       MOVE INL-KVAVIS   TO UT-KVAVIS                                     
250009       COMPUTE WS-DATE-DISPLAY   = 9999999999999999                       
250010                                    - INL-DAINLEV                         
250020       MOVE WS-DATE-DISPLAY(1:8)     TO UT-DAREGDAT                       
270005                                                                          
280002       PERFORM S11-WRITE-W5128J                                           
280003     ELSE                                                                 
280004       MOVE ZERO         TO UT-IDDISTR                                    
290002     END-IF                                                               
300002     .                                                                    
310002     EJECT                                                                
320002 Z-FINIT SECTION.                                                         
330002     CLOSE W5128J                                                         
340002     SKIP2                                                                
350002     MOVE 'S' TO POSTSUM-OPKOD                                            
360002     CALL POSTSUM USING POSTSUM-PARM                                      
370002     .                                                                    
380002     EJECT                                                                
390002 S11-WRITE-W5128J SECTION.                                                
400002                                                                          
410002     WRITE UT-RECORD FROM UT-AREA                                         
420002                                                                          
430002     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
440002     MOVE 'W5128J'   TO POSTSUM-FDNAMN                                    
450002     MOVE 'W5128JD1' TO POSTSUM-DDNAMN2                                   
460002     CALL POSTSUM USING POSTSUM-PARM                                      
470002     .                                                                    
480002     EJECT                                                                
490002 S99-ABEND SECTION.                                                       
500002                                                                          
510002     SKIP2                                                                
520002     MOVE 'S' TO POSTSUM-OPKOD                                            
530002     CALL POSTSUM USING POSTSUM-PARM                                      
540002     CALL ABEND USING RKOD-ABEND                                          
550002     .                                                                    
560002     EJECT                                                                
570002* --- IMS SECTIONS  ---                                                   
580002                                                                          
590002     EJECT                                                                
600002 IMS-GN-WDL6   SECTION.                                                   
610002                                                                          
620002     CALL CBLTDLI       USING GN WDL6-PCB DLI-IO-AREA                     
630002     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
640002     MOVE '  GAGKGB'       TO GOOD-STATUSCODES                            
650002     PERFORM IMS-STATUSCHECK                                              
660002     .                                                                    
670002     EJECT                                                                
680002 IMS-STATUSCHECK SECTION.                                                 
690002                                                                          
700002     SET STATUS-IX TO 1                                                   
710002     SEARCH GOOD-STATUS                                                   
720002       AT END                                                             
730002         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
740002           DELIMITED BY SIZE INTO ERROR-TEXT                              
750002         DISPLAY ERROR-TEXT                                               
760002         CALL FELLOG                                                      
770002       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
780002         CONTINUE                                                         
790002     END-SEARCH                                                           
800002     .                                                                    
