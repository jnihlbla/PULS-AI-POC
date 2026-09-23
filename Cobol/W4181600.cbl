001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W4181600.                                                
001200 AUTHOR.         SUSANNE OLSSON.                                          
001300 DATE-WRITTEN.   00/12/15.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        PROGRAMMET SKAPAR TVÅ FILER.                                     
001900*        1 UPPFÖLJNINGSFIL TILL LISTA SOM SKRIVS UT PÅ LDC'ER.            
001910*        2 RENSNINGSFIL                                                   
002000*                                                                         
002110*        PROGRAMMET LÄSER      WDR6   ( MED SB )                          
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003404     SKIP2                                                                
003405*          --- UPPFÖLJNINGSFIL LDC'ER.                                    
003410     SELECT W41816                     ASSIGN TO W41816D1.                
003420     SKIP2                                                                
003430*          --- RENSNINGSFIL                                               
003440     SELECT W41817                     ASSIGN TO W41816D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004007     SKIP3                                                                
004008 FD  W41816                                                               
004009     RECORDING       F                                                    
004010     BLOCK CONTAINS  0.                                                   
004011                                                                          
004020*01  POST -COPY W407R31A -PRE  UT1-  -L.                                  
004030     SKIP3                                                                
004040 FD  W41817                                                               
004050     RECORDING       F                                                    
004060     BLOCK CONTAINS  0.                                                   
004070                                                                          
004080*01  POST -COPY WDR601 -PRE  UT2-  -L.                                    
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W4181600'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007408     EJECT                                                                
007409 01  UT1-AREA-START              PIC X(24)   VALUE                        
007410                                 'UT1-AREA-START  '.                      
007411     SKIP2                                                                
007412                                                                          
007420*01  AREA -COPY W407R31A     -PRE UT1-                                    
007430     EJECT                                                                
007440 01  UT2-AREA-START              PIC X(24)   VALUE                        
007450                                 'UT2-AREA-START  '.                      
007460     SKIP2                                                                
007470                                                                          
007480*01  AREA -COPY WDR601     -PRE UT2-                                      
007490*    05   -COPY W407R31A   -PRE UT2- -RED UT2-FIL-WDR601-DATA             
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008201     03  W-WDR601KY-X.                                                    
008210         05  W-WDR601KY          PIC X(27)    VALUE SPACE.                
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR6'.                        
010002 01  DLI-IO-WDR6.                                                         
010010*    03  -COPY WDR601                                                     
010020*      05   -COPY W407R31A   -RED FIL-WDR601-DATA                         
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010601                                                                          
010602*01  -COPY W0008  -PRE WDR6-                                              
010610     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010801 PROCEDURE DIVISION  USING WDR6-PCB.                                      
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING WDR6-PCB.                                      
010900                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011300                                                                          
011401     PERFORM IMS-GET-WDR6                                                 
011402     PERFORM UNTIL SEGMENT-SAKNAS                                         
011405       IF FIL-CT-IDSYSTEM = 'W407' AND                                    
011406         (FIL-CT-IDPTYP   = 'R31')                                        
011407          PERFORM B-FLYTTA-DATA                                           
011408       END-IF                                                             
011411       PERFORM IMS-GET-WDR6                                               
011420     END-PERFORM                                                          
011500     PERFORM Z-FINIT                                                      
011600                                                                          
011700     MOVE ZERO TO RETURN-CODE                                             
011800     GOBACK                                                               
011900     .                                                                    
012000     EJECT                                                                
012100 A-INIT SECTION.                                                          
012301                                                                          
012302     OPEN OUTPUT W41816                                                   
012310                 W41817                                                   
012400                                                                          
012500     ACCEPT DAGENS-DATUM  FROM DATE                                       
012610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012800     .                                                                    
012900     EJECT                                                                
012910 B-FLYTTA-DATA SECTION.                                                   
012920*                                                                         
012930*---FLYTTAR ALLA POSTER TILL UT1- OCH UT2-AREAN                           
012940*---SKRIVER SEDAN POSTER PÅ W41816 OCH W41817                             
012950*                                                                         
012960     MOVE DLI-IO-WDR6     TO UT2-AREA                                     
012970     MOVE UT2-FIL-WDR601-DATA TO UT1-AREA                                 
012980                                                                          
012990     PERFORM S11-SKRIV-W41816                                             
012991     PERFORM S12-SKRIV-W41817                                             
012992     .                                                                    
012993     EJECT                                                                
013000 Z-FINIT SECTION.                                                         
013101     CLOSE W41816                                                         
013110           W41817                                                         
013201     SKIP2                                                                
013202     MOVE 'S' TO POSTSUM-OPKOD                                            
013210     CALL POSTSUM USING POSTSUM-PARM                                      
013300     .                                                                    
013501     EJECT                                                                
013502 S11-SKRIV-W41816 SECTION.                                                
013503                                                                          
013504     WRITE UT1-POST FROM UT1-AREA                                         
013505                                                                          
013506     MOVE 'UT1-'     TO POSTSUM-TRANSTYP                                  
013507     MOVE 'W41816'   TO POSTSUM-FDNAMN                                    
013508     MOVE 'W41816D1' TO POSTSUM-DDNAMN2                                   
013509     CALL POSTSUM USING POSTSUM-PARM                                      
013510     .                                                                    
013511     EJECT                                                                
013512 S12-SKRIV-W41817 SECTION.                                                
013513                                                                          
013514     WRITE UT2-POST FROM UT2-AREA                                         
013515                                                                          
013517     MOVE 'UT2-'     TO POSTSUM-TRANSTYP                                  
013518     MOVE 'W41817'   TO POSTSUM-FDNAMN                                    
013519     MOVE 'W41816D2' TO POSTSUM-DDNAMN2                                   
013520     CALL POSTSUM USING POSTSUM-PARM                                      
013530     .                                                                    
013700     EJECT                                                                
013800* S99-ABEND SECTION.                                                      
013900*                                                                         
014001*    SKIP2                                                                
014002*    MOVE 'S' TO POSTSUM-OPKOD                                            
014010*    CALL POSTSUM USING POSTSUM-PARM                                      
014100*    CALL ABEND USING RKOD-ABEND                                          
014200*    .                                                                    
014300*    EJECT                                                                
014400* --- IMS SEKTIONER ---                                                   
014500                                                                          
014601                                                                          
014602 IMS-GET-WDR6   SECTION.                                                  
014603                                                                          
014604     CALL CBLTDLI USING GN WDR6-PCB DLI-IO-WDR6                           
014605     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
014606     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014607     PERFORM IMS-STATUSKONTROLL                                           
014610     .                                                                    
014700     EJECT                                                                
014800 IMS-STATUSKONTROLL SECTION.                                              
014900                                                                          
015000     SET STATUS-IX TO 1                                                   
015100     SEARCH GODK-STATUS                                                   
015200       AT END                                                             
015300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015400           DELIMITED BY SIZE INTO FELTEXT                                 
015500         DISPLAY FELTEXT                                                  
015600         CALL FELLOG                                                      
015700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015800         CONTINUE                                                         
015900     END-SEARCH                                                           
016000     .                                                                    
