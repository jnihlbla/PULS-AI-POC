000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4799700.                                                
000400*AUTHOR.         STEFANO GIOBBI.                                          
000500*DATE-WRITTEN.   920520.                                                  
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SB-PROGRAM SOM LÄSER NED WDQ2 FÖR SKAPANDET AV ETT               
001100*        PRE-EXTRAKT.                                                     
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLORQI (WDQ2)                              
001400*                                                                         
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- PRE-EXTRAKT AV WDQ2                                        
002300     SELECT W47997                     ASSIGN TO W47997D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W47997                                                               
003000     RECORDING       V                                                    
003100     BLOCK CONTAINS  0.                                                   
003200     SKIP2                                                                
003300*01  POST -COPY W479973  -PRE U97-   -L.                                  
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600     SKIP2                                                                
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(8)    VALUE 'W4799700'.            
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  ARB-IX                      PIC S9(3)   VALUE +0  COMP-3.            
004300 77  ARB-IX-MAX-99               PIC S9(2)   VALUE +99 COMP-3.            
004400 77  WS-WRITE-Q212-21            PIC X       VALUE 'N'.                   
004500     EJECT                                                                
004600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004700 01  FILLER REDEFINES DAGENS-DATUM.                                       
004800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005100     EJECT                                                                
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300*                                                                         
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005800     SKIP2                                                                
005900*    --- PARAMETRAR TILL ABEND                                            
006000                                                                          
006100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006300     EJECT                                                                
006400*    --- PARAMETRAR TILL POSTSUM                                          
006500*                                                                         
006600*01  -COPY W0005   -PRE  POSTSUM-                                         
006700     EJECT                                                                
006800 01  U97-AREA-START              PIC X(24)   VALUE                        
006900                                 'U97-AREA-START  '.                      
007000     SKIP2                                                                
007100 01  U97-AREA.                                                            
007200                                                                          
007300     03  U97-IDSEGM            PIC X(6)   VALUE SPACE.                    
007400     03  FILLER                PIC X(500) VALUE SPACE.                    
007500     SKIP2                                                                
007600*01  FILLER   -PRE U97Q201- -COPY W479971 -RED U97-AREA.                  
007700     EJECT                                                                
007800*01  FILLER   -PRE U97Q212- -COPY W479973 -RED U97-AREA.                  
007900     EJECT                                                                
008000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008100*                                                                         
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008400     SKIP3                                                                
008500 01  NYCKLAR-TILL-DLI.                                                    
008600     03  W-IDORDER-X.                                                     
008700         05  W-IDORDER           PIC S9(4)   VALUE ZERO COMP-3.           
008800     03  W-IDLEVNR-X.                                                     
008900         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
009000     03  W-IDDC-X.                                                        
009100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
009200     SKIP2                                                                
009300*    --- STATUS-KOD FRÅN IMS                                              
009400 01  STATUS-WS                   PIC XX.                                  
009500     88  SEGMENT-FINNS                       VALUE '  '.                  
009600     88  BASEN-SLUT                          VALUE 'GB'.                  
009700     SKIP2                                                                
009800 01  GODK-STATUSKODER.                                                    
009900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010000     SKIP3                                                                
010100 01  SSA1                        PIC X(64).                               
010200 01  SSA2                        PIC X(64).                               
010300     EJECT                                                                
010400*    --- IMS FUNKTIONSKODER                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010700*    ---  DLI INPUT-OUTPUT AREA                                           
010800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010900     SKIP3                                                                
011000 01  DLI-IO-AREA.                                                         
011100     03  IO-AREA                 PIC X(4000)  VALUE SPACE.                
011200     SKIP3                                                                
011300     03  WLORQI01 REDEFINES IO-AREA.                                      
011400*        05  -COPY WDQ201                                                 
011500     EJECT                                                                
011600     03  WLORQI12 REDEFINES IO-AREA.                                      
011700*        05  -COPY WDQ212                                                 
011800     EJECT                                                                
011900     03  WLORQI21 REDEFINES IO-AREA.                                      
012000*        05  -COPY WDQ221                                                 
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300                                                                          
012400     EJECT                                                                
012500*01  -COPY W0008  -PRE ORQI-                                              
012600     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012800 PROCEDURE DIVISION  USING ORQI-PCB.                                      
012900     ENTRY 'DLITCBL' USING ORQI-PCB.                                      
013000                                                                          
013100     PERFORM A-INIT                                                       
013200                                                                          
013300     PERFORM IMS-GET-ORQI                                                 
013400     PERFORM UNTIL BASEN-SLUT                                             
013500                                                                          
013600       EVALUATE ORQI-SEG-NAME-FB                                          
013700         WHEN 'WDQ201'                                                    
013800           IF WS-WRITE-Q212-21 = JA                                       
013900              PERFORM S12-SKRIV-W47997                                    
014000           END-IF                                                         
014100           PERFORM B-FLYTTA-WDQ201-INFO                                   
014200           PERFORM S11-SKRIV-W47997                                       
014210           MOVE NEJ TO WS-WRITE-Q212-21                                   
014300         WHEN 'WDQ212'                                                    
014400           PERFORM D-FLYTTA-WDQ212-INFO                                   
014410           MOVE JA  TO WS-WRITE-Q212-21                                   
014500         WHEN 'WDQ221'                                                    
014510           MOVE LOR-IDPRC TO U97Q212-IDPRC(LOR-ADLAGOMR)                  
014700       END-EVALUATE                                                       
014800                                                                          
014900       PERFORM IMS-GET-ORQI                                               
015000                                                                          
015100     END-PERFORM                                                          
015200                                                                          
015400     IF WS-WRITE-Q212-21 = JA                                             
015500         PERFORM S12-SKRIV-W47997                                         
015600     END-IF                                                               
015700                                                                          
015800     PERFORM Z-FINIT                                                      
015900                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INIT SECTION.                                                          
016500                                                                          
016600     OPEN OUTPUT W47997                                                   
016700                                                                          
016800     ACCEPT DAGENS-DATUM  FROM DATE                                       
016900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017200     .                                                                    
017300     EJECT                                                                
017400 B-FLYTTA-WDQ201-INFO SECTION.                                            
017500                                                                          
017600     MOVE 'WDQ201'      TO U97Q201-IDSEGM                                 
017700     MOVE OHUV-IDORDER  TO U97Q201-IDORDER                                
017800     MOVE OHUV-FLKLAR   TO U97Q201-FLKLAR                                 
017900     MOVE OHUV-KDORDKL  TO U97Q201-KDORDKL                                
018000     MOVE OHUV-IDUSER   TO U97Q201-IDUSER                                 
018100     MOVE OHUV-IDSYSTEM TO U97Q201-IDSYSTEM                               
018200     MOVE OHUV-TIREPDAT TO U97Q201-TIREPDAT                               
018500     .                                                                    
018600     EJECT                                                                
018700 D-FLYTTA-WDQ212-INFO SECTION.                                            
018800                                                                          
018900     MOVE 'WDQ212'     TO U97Q212-IDSEGM                                  
019000     MOVE ARB-IDDC     TO U97Q212-IDDC                                    
019100     MOVE ARB-KDFRAKT  TO U97Q212-KDFRAKT                                 
019200                                                                          
019300     MOVE +1           TO ARB-IX                                          
019400     PERFORM UNTIL ARB-IX > ARB-IX-MAX-99                                 
019500       MOVE ZERO       TO U97Q212-IDPRC(ARB-IX)                           
019600       ADD  +1         TO ARB-IX                                          
019700     END-PERFORM                                                          
019900     .                                                                    
020000     EJECT                                                                
020900 Z-FINIT SECTION.                                                         
021000                                                                          
021100     CLOSE W47997                                                         
021200     SKIP2                                                                
021300     MOVE 'S' TO POSTSUM-OPKOD                                            
021400     CALL POSTSUM USING POSTSUM-PARM                                      
021500     .                                                                    
021600     EJECT                                                                
021700 S11-SKRIV-W47997 SECTION.                                                
021800                                                                          
021900***  WRITE INFORMATION FROM WDQ201                                        
022100     WRITE U97-POST FROM U97Q201-W479971                                  
022200                                                                          
022300     MOVE 'U97'      TO    POSTSUM-TRANSTYP                               
022400     MOVE 'W47997'   TO    POSTSUM-FDNAMN                                 
022500     MOVE 'W47997D1' TO    POSTSUM-DDNAMN2                                
022600     CALL  POSTSUM   USING POSTSUM-PARM                                   
022700     .                                                                    
022800     EJECT                                                                
022900 S12-SKRIV-W47997 SECTION.                                                
023000                                                                          
023100***  WRITE INFORMATION FROM WDQ212 AND WDQ221                             
023300     WRITE U97-POST FROM U97Q212-W479973                                  
023400                                                                          
023500     MOVE 'U97'      TO    POSTSUM-TRANSTYP                               
023600     MOVE 'W47997'   TO    POSTSUM-FDNAMN                                 
023700     MOVE 'W47997D1' TO    POSTSUM-DDNAMN2                                
023800     CALL  POSTSUM   USING POSTSUM-PARM                                   
023900     .                                                                    
024000     EJECT                                                                
024100*                                                                         
024200*                                                                         
024300*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
024400*                 III     III MM MMMMM MM SSSS   SSSS                     
024500*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
024600*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
024700*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
024800*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
024900*                 III     III MM MMMMM MM SSSS   SSSS                     
025000*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
025100*                                                                         
025200*                                                                         
025300 IMS-GET-ORQI   SECTION.                                                  
025400                                                                          
025500     CALL    CBLTDLI          USING GN ORQI-PCB                           
025600                                    DLI-IO-AREA                           
025700     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
025800     MOVE    '  GAGKGB'       TO    GODK-STATUSKODER                      
025900     PERFORM IMS-STATUSKONTROLL                                           
026000     .                                                                    
026100     EJECT                                                                
026200 IMS-STATUSKONTROLL SECTION.                                              
026300                                                                          
026400     SET    STATUS-IX TO 1                                                
026500     SEARCH GODK-STATUS                                                   
026600       AT END                                                             
026700         CALL FELLOG                                                      
026800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026900         CONTINUE                                                         
027000     END-SEARCH                                                           
027100     .                                                                    
