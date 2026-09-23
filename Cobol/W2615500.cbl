001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W2615500.                                                
001300 AUTHOR.         CONNY EGHOLT.                                            
001400 DATE-WRITTEN.   01/10/08.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        SKAPAR EN EXTRAKTFIL FRÅN WDQ4 FÖR MATCHNING MOT FIL MED         
001900*        EFRRAD-INFO                                                      
001910*        I RUTIN W261V1, SOM SKALL RAPPORTERA VILKA SKROTORDRADE          
001920*        ARTIKLAR SOM FORTFARANDE INTE HAR SKICKATS IVÄG FÖR SKROT        
002000*                                                                         
002110*        PROGRAMMET LÄSER      WDQ4                                       
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
003401     SKIP2                                                                
003402*          --- WDQ4-EXTRACT                                               
003410     SELECT W26155                     ASSIGN TO W26155D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W26155                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005                                                                          
004010*01  POST -COPY WDQ401 -PRE  UT-  -L.                                     
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W2615500'.            
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
007401     EJECT                                                                
007402 01  UT-AREA-START        PIC X(24)   VALUE 'UT-AREA-START  '.            
007410*01  AREA -COPY WDQ401     -PRE UT-                                       
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008201     03  W-WDQ401KY-X.                                                    
008210         05  W-WDQ401KY          PIC X(20)    VALUE SPACE.                
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
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ4'.                        
010002 01  DLI-IO-WDQ4.                                                         
010010*    03  -COPY WDQ401                                                     
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010601                                                                          
010602*01  -COPY W0008  -PRE WDQ4-                                              
010610     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010801 PROCEDURE DIVISION  USING WDQ4-PCB.                                      
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING WDQ4-PCB.                                      
010900                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011300                                                                          
011401     PERFORM IMS-GET-WDQ4                                                 
011402     PERFORM UNTIL SEGMENT-SAKNAS                                         
011403       EVALUATE WDQ4-SEG-NAME-FB                                          
011404         WHEN 'WDQ401'                                                    
011405           MOVE DLI-IO-WDQ4  TO UT-AREA                                   
011406           PERFORM S11-SKRIV-W26155                                       
011407       END-EVALUATE                                                       
011408       PERFORM IMS-GET-WDQ4                                               
011410     END-PERFORM                                                          
011500     PERFORM Z-FINIT                                                      
011600                                                                          
011700     MOVE ZERO TO RETURN-CODE                                             
011800     GOBACK                                                               
011900     .                                                                    
012000     EJECT                                                                
012100 A-INIT SECTION.                                                          
012301                                                                          
012310     OPEN OUTPUT W26155                                                   
012400                                                                          
012500     ACCEPT DAGENS-DATUM  FROM DATE                                       
012610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012800     .                                                                    
012900     EJECT                                                                
013000 Z-FINIT SECTION.                                                         
013110     CLOSE W26155                                                         
013201     SKIP2                                                                
013202     MOVE 'S' TO POSTSUM-OPKOD                                            
013210     CALL POSTSUM USING POSTSUM-PARM                                      
013300     .                                                                    
013501     EJECT                                                                
013502 S11-SKRIV-W26155 SECTION.                                                
013503                                                                          
013504     WRITE UT-POST FROM UT-AREA                                           
013505                                                                          
013506     MOVE 'UT '     TO POSTSUM-TRANSTYP                                   
013507     MOVE 'W26155' TO POSTSUM-FDNAMN                                      
013508     MOVE 'W26155D1' TO POSTSUM-DDNAMN2                                   
013509     CALL POSTSUM USING POSTSUM-PARM                                      
013510     .                                                                    
013700     EJECT                                                                
014400* --- IMS SEKTIONER ---                                                   
014500                                                                          
014601                                                                          
014602 IMS-GET-WDQ4   SECTION.                                                  
014603                                                                          
014604     CALL CBLTDLI USING GN WDQ4-PCB DLI-IO-WDQ4                           
014605     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
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
