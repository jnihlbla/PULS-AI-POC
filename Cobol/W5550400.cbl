000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5550400.                                                
000400*AUTHOR.         MARKUS ASPFJÄLL.                                         
000500*DATE-WRITTEN.   98/01/27.                                                
000600*DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER INDATA PARAMETRAR IFRÅN SOP                     
001400*                   OCH SKAPAR EN UTFIL MED DESSA PARAMETRAR              
001600*        PROGRAMMET                                                       
001601*        - SKAPAR EN UTFIL OCH LÄGGER TILL POSTER UNDER DAGEN             
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002810*          --- SYSIN FRÅN JCL                                             
002830      SELECT INDATA          ASSIGN TO SYSIN.                             
002840                                                                          
003092                                                                          
003095*          --- UTFIL W55504                                               
003096     SELECT W55504           ASSIGN TO W55504D1.                          
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003510 FD INDATA                                                                
003520     LABEL RECORD STANDARD                                                
003530     RECORDING  F                                                         
003540     BLOCK CONTAINS 0.                                                    
003560 01  INPOST                  PIC X(80).                                   
004010                                                                          
004011 FD  W55504                                                               
004012     RECORDING       F                                                    
004013     BLOCK CONTAINS  0.                                                   
004014 01  W55504-REC         PIC X(16).                                        
004015                                                                          
004120     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004301*    -- CHECKED BY WY2000                                                 
004310     SKIP3                                                                
004400 77  IDPGM                       PIC X(8)    VALUE 'W5550400'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  INDATA-EOF                  PIC X       VALUE 'N'.                   
004720     EJECT                                                                
004721 01  FILLER                      PIC X(10)   VALUE 'INAREA'.              
004722 01  INAREA.                                                              
004725   03    PRM-IDARTNR           PIC S9(9).                                 
004728   03    FILLER                PIC X.                                     
004729   03    PRM-FLTYEAR           PIC X.                                     
004730   03    FILLER                PIC X.                                     
004731   03    PRM-FLLYEAR           PIC X.                                     
004732   03    FILLER                PIC X.                                     
004739   03    PRM-IDUSER            PIC X(8).                                  
004740   03    FILLER                PIC X(65).                                 
004741******POSTEN SOM KOMMER IFRÅN SOP ÄR 80 BYTE,DÄRFÖR EN FILLER PÅ          
004742******64 BYTE***************************************************          
004747     EJECT                                                                
004748                                                                          
004780 01  ANT-INS-DEL.                                                         
004781     03  ANT-INS                 PIC  9(13)  VALUE ZERO.                  
004782     03  ANT-DEL                 PIC  9(13)  VALUE ZERO.                  
004783     03  ANT-TOT                 PIC  9(13)  VALUE ZERO.                  
004784                                                                          
004785 01  FELTEXT.                                                             
004786     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004787     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004824                                                                          
004825 01  WS-DATUM.                                                            
004826     03  WS-SEKEL            PIC 9(2).                                    
004827     03  WS-DAT.                                                          
004828      05 WS-AARTAL           PIC 9(2).                                    
004829      05 FILLER              PIC 9(4).                                    
004830 01  WS-DATUM-N  REDEFINES WS-DATUM   PIC 9(8).                           
004831                                                                          
004832 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004833 01  WS-DAGENS-DATUM.                                                     
004834   03  WS-DAGENS-AAR             PIC 9(2).                                
004835   03  WS-DAGENS-MAN             PIC 9(2).                                
004836   03  WS-DAGENS-DAG             PIC 9(2).                                
004837 01  WS-DAGENS-DATUM-US.                                                  
004838   03  WS-DAGENS-MAN-US          PIC 9(2).                                
004839   03  WS-DAGENS-DAG-US          PIC 9(2).                                
004840   03  WS-DAGENS-AAR-US          PIC 9(2).                                
004841 01  DAGENS-KLOCKA               PIC 9(8)    VALUE ZERO.                  
004850                                                                          
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006010     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006020     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006040     SKIP2                                                                
006050*    --- PARAMETRAR TILL ABEND                                            
006060                                                                          
006070 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006080 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006090 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006091     SKIP2                                                                
006100                                                                          
006200     EJECT                                                                
006300*    --- PARAMETRAR TILL POSTSUM                                          
006400*                                                                         
006500*01  -COPY W0005   -PRE  POSTSUM-                                         
006600     EJECT                                                                
006601*    --- PARAMETRAR TILL WDATKONV                                         
006602*                                                                         
006610*01  -COPY WDATAREA                                                       
006620     EJECT                                                                
007000*                                                                         
007101 01  FILLER                    PIC X(24)   VALUE 'UT-AREA'.               
007102                                                                          
007110*01  POST -COPY W55504 -PRE W55504-                                       
007120     EJECT                                                                
007200 01  FILLER                    PIC X(16) VALUE 'IMS-WS'.                  
007300                                                                          
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008800     88  IMS-EJ-OK                           VALUE 'XD'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(128).                              
009400 01  SSA2                        PIC X(128).                              
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010100     SKIP3                                                                
010200     EJECT                                                                
010300                                                                          
011400 LINKAGE SECTION.                                                         
011500                                                                          
011600*01  -COPY W0009  -PRE MSG-                                               
011700                                                                          
012000     EJECT                                                                
012400 PROCEDURE DIVISION  USING MSG-PCB.                                       
012410 MAIN SECTION.                                                            
012500     ENTRY 'DLITCBL' USING MSG-PCB.                                       
012800                                                                          
012900     PERFORM A-INIT                                                       
012910     PERFORM B-BYGG-UTPOST                                                
013700     PERFORM S01-SKRIV-W55504                                             
017100                                                                          
017300     PERFORM Z-FINIT                                                      
017400                                                                          
017500     MOVE ZERO TO RETURN-CODE                                             
017600     GOBACK                                                               
017700     .                                                                    
017800     EJECT                                                                
017900 A-INIT SECTION.                                                          
018210     OPEN INPUT INDATA                                                    
018220     READ INDATA NEXT RECORD INTO INAREA                                  
018230       AT END MOVE JA               TO INDATA-EOF                         
018240     END-READ                                                             
018250     CLOSE INDATA                                                         
018260                                                                          
018270     UNSTRING INAREA DELIMITED BY SPACE INTO PRM-IDARTNR                  
018280                                             PRM-FLTYEAR                  
018281                                             PRM-FLLYEAR                  
018292                                             PRM-IDUSER                   
018294                                                                          
018313     OPEN OUTPUT W55504                                                   
018314                                                                          
018400                                                                          
018500     ACCEPT DAGENS-DATUM  FROM DATE                                       
018501     MOVE DAGENS-DATUM    TO WS-DAGENS-DATUM                              
018510     ACCEPT DAGENS-KLOCKA FROM TIME                                       
018600                                                                          
018602     MOVE DAGENS-DATUM   TO WS-DAT                                        
018603                            WS-DAGENS-DATUM                               
018604                                                                          
018606     IF WS-AARTAL < 50                                                    
018607       MOVE 20           TO WS-SEKEL                                      
018608     ELSE                                                                 
018609       MOVE 19           TO WS-SEKEL                                      
018610     END-IF                                                               
018613                                                                          
018615     MOVE WS-DAGENS-AAR  TO WS-DAGENS-AAR-US                              
018616     MOVE WS-DAGENS-MAN  TO WS-DAGENS-MAN-US                              
018617     MOVE WS-DAGENS-DAG  TO WS-DAGENS-DAG-US                              
018620                                                                          
018700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018701                                                                          
018800     .                                                                    
018900     EJECT                                                                
019000 B-BYGG-UTPOST SECTION.                                                   
019010     IF PRM-FLTYEAR = 'Y' AND PRM-FLLYEAR = 'Y'                           
019020        MOVE 1 TO W55504-BEST-FLSORT                                      
019021        PERFORM BA-FLYTTA-VARDEN                                          
019022        PERFORM S01-SKRIV-W55504                                          
019023        MOVE 2 TO W55504-BEST-FLSORT                                      
019025     ELSE                                                                 
019040        IF PRM-FLTYEAR = 'Y' AND PRM-FLLYEAR = 'N'                        
019050           MOVE 1 TO W55504-BEST-FLSORT                                   
019060        ELSE                                                              
019070           MOVE 2 TO W55504-BEST-FLSORT                                   
019080        END-IF                                                            
019081        PERFORM BA-FLYTTA-VARDEN                                          
019090     END-IF                                                               
019091     .                                                                    
019092     EJECT                                                                
019093                                                                          
019094 BA-FLYTTA-VARDEN SECTION.                                                
019100     MOVE PRM-IDARTNR         TO W55504-BEST-IDARTNR                      
019110     MOVE PRM-FLTYEAR         TO W55504-BEST-FLTYEAR                      
019120     MOVE PRM-FLLYEAR         TO W55504-BEST-FLLYEAR                      
019140     MOVE PRM-IDUSER          TO W55504-BEST-IDUSER                       
019200     .                                                                    
019300     EJECT                                                                
019400                                                                          
019439 Z-FINIT SECTION.                                                         
019695     CLOSE W55504                                                         
019740     .                                                                    
019800     EJECT                                                                
021210 S01-SKRIV-W55504 SECTION.                                                
021220                                                                          
021230     WRITE W55504-REC FROM W55504-BEST-W55504                             
021240                                                                          
021241     MOVE 'W55504'    TO POSTSUM-FDNAMN                                   
021242     MOVE 'W55504D1'  TO POSTSUM-DDNAMN2                                  
021243     MOVE 'UT  '      TO POSTSUM-TRANSTYP                                 
021260     CALL POSTSUM USING POSTSUM-PARM                                      
021270     .                                                                    
021280     EJECT                                                                
027706                                                                          
027790 S99-ABEND SECTION.                                                       
027800                                                                          
027900     SKIP2                                                                
028000     DISPLAY '???????'                                                    
028100     MOVE 'S' TO POSTSUM-OPKOD                                            
028200     CALL POSTSUM USING POSTSUM-PARM                                      
028300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
028400     .                                                                    
