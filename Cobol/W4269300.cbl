001000 ID DIVISION.                                                             
001100                                                                          
001200 PROGRAM-ID.     W4269300.                                                
001300 AUTHOR.         KJELL ANDRE.                                             
001400 DATE-WRITTEN.   96/12/16.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        LÄSER EK-TRANSAR FRÅN WDR3 (W60206) OCH SKRIVER UT DEM           
001900*        PÅ FIL. EN LOGG-FIL ÖVER UTLÄSTA TRANSAR SKRIVS OCKSÅ            
002000*        FÖR BORTTAG I ETT EFTERFÖLJANDE BMP-PROGRAM.                     
002101*                                                                         
002110*        PROGRAMMET LÄSER DATABAS WLFILC (WDR3)                           
002200*                                                                         
002300                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- EK-TRANSAR                                                 
003403     SELECT W42693                     ASSIGN TO W42693D1.                
003404     SKIP2                                                                
003408*          --- LOGG PÅ TRANSAR SOM SKA TAS BORT                           
003410     SELECT W42694                     ASSIGN TO W42693D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W42693                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005                                                                          
004006*01  POST -COPY W426PEK  -PRE EK-   -L.                                   
004007     SKIP3                                                                
004014 FD  W42694                                                               
004015     RECORDING       F                                                    
004016     BLOCK CONTAINS  0.                                                   
004017                                                                          
004020*01  POST -COPY WDR301   -PRE  LOGG-  -L.                                 
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004310*    -- CHECKED BY WY2000                                                 
004320                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W4269300'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  ELLER                       PIC X       VALUE '!'.                   
004900     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007307 01  EK-AREA-START               PIC X(24)   VALUE                        
007308                                 'EK-AREA-START   '.                      
007309     SKIP2                                                                
007311*01  AREA -COPY W426PEK     -PRE EK-                                      
007312     EJECT                                                                
007313 01  LOGG-AREA-START             PIC X(24)   VALUE                        
007314                                 'LOGG-AREA-START  '.                     
007315     SKIP2                                                                
007320*01  AREA -COPY WDR301     -PRE LOGG-                                     
007400     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008101     03  W-IDCPYTXT-EK-X         PIC X(8)    VALUE 'W426PEK '.            
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008700     88  SEGMENT-SAKNAS                      VALUE 'GE' 'GB'.             
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010001 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLFILC01'.                    
010002 01  DLI-IO-WLFILC01.                                                     
010010*    03  -COPY WDR301  -PRE FILC-                                         
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010601     EJECT                                                                
010602*01  -COPY W0008  -PRE FILC-                                              
010610     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010801 PROCEDURE DIVISION  USING FILC-PCB.                                      
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING FILC-PCB.                                      
010900                                                                          
011110*    -- LÄS ALLA EK-TRANSAR OCH SKRIV UT DEM                              
011200     PERFORM A-INIT                                                       
011300     PERFORM IMS-GET-FILC-FIL                                             
011400     PERFORM UNTIL SEGMENT-SAKNAS                                         
011500                                                                          
011501       EVALUATE FILC-FIL-IDCPYTXT                                         
011502       WHEN 'W426PEK '                                                    
011510         MOVE FILC-FIL-WDR301-DATA TO EK-AREA                             
011600         PERFORM S11-SKRIV-W42693-EK                                      
011601                                                                          
011602         MOVE FILC-FIL-WDR301 TO LOGG-AREA                                
011603         PERFORM S13-SKRIV-W42694-LOGG                                    
011604                                                                          
011605       END-EVALUATE                                                       
011606                                                                          
012100       PERFORM IMS-GET-FILC-FIL                                           
012200     END-PERFORM                                                          
012300                                                                          
012400                                                                          
012500     PERFORM Z-FINIT                                                      
012600                                                                          
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013000     EJECT                                                                
013100 A-INIT SECTION.                                                          
013301                                                                          
013302     OPEN OUTPUT W42693                                                   
013310                 W42694                                                   
013400                                                                          
013610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013800     .                                                                    
013900     EJECT                                                                
014000 Z-FINIT SECTION.                                                         
014101     CLOSE W42693                                                         
014110           W42694                                                         
014201     SKIP2                                                                
014202     MOVE 'S' TO POSTSUM-OPKOD                                            
014210     CALL POSTSUM USING POSTSUM-PARM                                      
014300     .                                                                    
014501     EJECT                                                                
014502 S11-SKRIV-W42693-EK    SECTION.                                          
014503                                                                          
014504     WRITE EK-POST  FROM EK-AREA                                          
014505                                                                          
014506     MOVE 'EK '      TO POSTSUM-TRANSTYP                                  
014507     MOVE 'W42693'   TO POSTSUM-FDNAMN                                    
014508     MOVE 'W42693D1' TO POSTSUM-DDNAMN2                                   
014509     CALL POSTSUM USING POSTSUM-PARM                                      
014510     .                                                                    
014511     EJECT                                                                
014522 S13-SKRIV-W42694-LOGG SECTION.                                           
014523                                                                          
014524     WRITE LOGG-POST FROM LOGG-AREA                                       
014525                                                                          
014526     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014527     MOVE 'W42694'   TO POSTSUM-FDNAMN                                    
014528     MOVE 'W42693D2' TO POSTSUM-DDNAMN2                                   
014529     CALL POSTSUM USING POSTSUM-PARM                                      
014530     .                                                                    
014700     EJECT                                                                
015400* --- IMS SEKTIONER ---                                                   
015500     SKIP3                                                                
015602 IMS-GET-FILC-FIL SECTION.                                                
015603                                                                          
015604     STRING 'WLFILC01(IDCPYTXT =' W-IDCPYTXT-EK-X  ')'                    
015607          DELIMITED BY SIZE INTO SSA1                                     
015608     MOVE '  GEGB' TO GODK-STATUSKODER                                    
015609     CALL CBLTDLI USING GN FILC-PCB DLI-IO-WLFILC01 SSA1                  
015610     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
015611     PERFORM IMS-STATUSKONTROLL                                           
015620     .                                                                    
015700     EJECT                                                                
015800 IMS-STATUSKONTROLL SECTION.                                              
015900                                                                          
016000     SET STATUS-IX TO 1                                                   
016100     SEARCH GODK-STATUS                                                   
016200       AT END                                                             
016300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016400           DELIMITED BY SIZE INTO FELTEXT                                 
016500         DISPLAY FELTEXT                                                  
016600         CALL FELLOG                                                      
016700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016800         CONTINUE                                                         
016900     END-SEARCH                                                           
017000     .                                                                    
