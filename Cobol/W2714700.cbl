000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2714700.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   95/05/22.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER WDK7 MED SB                                                
001000*        SKAPAR FILER MED SAMTLIGA DATAELEMENT FRÅN WDK7                  
001100*        FILERNA ANVÄNDS AV REFILLSYSTEMET VID PERIODKÖRNING              
001200*        DÅ BEFINTLIG GRUNDFIL EJ INNEHÅLLER AKTUELLT DATA.               
001300*                                                                         
001400*        EN FIL FÖR VART LAGER                                            
001410*        LDC         W011.LDC.W27147                                      
001420*        SDC         W011.SDC.W27147                                      
001430*        NDC         W011.NDC.W27147                                      
001440*                                                                         
001450*       FUNGERAR  FÖR SDC 21, 22, 23, 24, 25, 26                          
001460*                     NDC 41, 42, 43, 51, 61, 62                          
001470*                     LDC 1A, 1B, 1C, 1K,                                 
001480*                         2A, 2B, 2C, 2D, 2E, 2F,                         
001490*                         2G, 2H, 2I, 2J, 2K, 2L,                         
001491*                         2M, 2N, 2O, 3A, 3B, 3C, 3D,                     
001492*                         3E, 3F, 3G, 3H, 3I                              
001493*                                                                         
005500*    ABENDKODER:                                                          
005600*        U0016 -  . . . .                                                 
005700*        U1000 -  . . . .                                                 
005800*                                                                         
005900                                                                          
006000     SKIP3                                                                
006100 ENVIRONMENT DIVISION.                                                    
006200     SKIP2                                                                
006300 INPUT-OUTPUT SECTION.                                                    
006400                                                                          
006500 FILE-CONTROL.                                                            
006600     SKIP2                                                                
006700*          --- SAMTLIGA DATAELEMENT FRÅN WDK711, IDDC = LDC               
006800     SELECT LDC-W27147                ASSIGN TO W27147D1.                 
006900                                                                          
007000*          --- SAMTLIGA DATAELEMENT FRÅN WDK711, IDDC = SDC               
007100     SELECT SDC-W27147                ASSIGN TO W27147D2.                 
007200                                                                          
007300*          --- SAMTLIGA DATAELEMENT FRÅN WDK711, IDDC = NDC               
007400     SELECT NDC-W27147                ASSIGN TO W27147D3.                 
007500                                                                          
007600*          --- INTERN REFILL NDC-CN                                       
007700     SELECT CHN-W27147                ASSIGN TO W27147D4.                 
007800                                                                          
015700     EJECT                                                                
015800 DATA DIVISION.                                                           
015900     SKIP2                                                                
016000 FILE SECTION.                                                            
016100     SKIP3                                                                
016200 FD  LDC-W27147                                                           
016300     RECORDING       F                                                    
016400     BLOCK CONTAINS  0.                                                   
016500                                                                          
016600*01  POST -COPY W27147 -PRE  LDC-W27147-  -L.                             
016700                                                                          
016800 FD  SDC-W27147                                                           
016900     RECORDING       F                                                    
017000     BLOCK CONTAINS  0.                                                   
017100                                                                          
017200*01  POST -COPY W27147 -PRE  SDC-W27147-  -L.                             
017300                                                                          
017400 FD  NDC-W27147                                                           
017500     RECORDING       F                                                    
017600     BLOCK CONTAINS  0.                                                   
017700                                                                          
017800*01  POST -COPY W27147 -PRE  NDC-W27147-   -L.                            
017900                                                                          
018000 FD  CHN-W27147                                                           
018100     RECORDING       F                                                    
018200     BLOCK CONTAINS  0.                                                   
018300                                                                          
018400*01  POST -COPY W27147 -PRE  CHN-W27147-   -L.                            
018500                                                                          
034010     EJECT                                                                
034100 WORKING-STORAGE SECTION.                                                 
034400*    -- CHECKED BY WY2000                                                 
034500 77  IDPGM                       PIC X(8)    VALUE 'W2714700'.            
034600 77  JA                          PIC X       VALUE 'J'.                   
034700 77  NEJ                         PIC X       VALUE 'N'.                   
034800                                                                          
034900 01  ARBETSAREOR.                                                         
035000     03 WS-SPAR-IDARTNR          PIC S9(9)   VALUE ZERO COMP-3.           
035100     03 IX                       PIC S9(3)   VALUE ZERO COMP-3.           
035300                                                                          
035400     EJECT                                                                
035500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
035600 01  FILLER REDEFINES DAGENS-DATUM.                                       
035700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
035800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
035900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
036000     EJECT                                                                
036100*      --- VALID IDDC CODES                                               
036200*                                                                         
036300*01    -COPY WWDC99                                                       
036400       EJECT                                                              
036500 01  DYNAMISKA-SUBPROGRAM.                                                
036600*                                                                         
036700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
036800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
036900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
037000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
037100     SKIP2                                                                
037200*    --- PARAMETRAR TILL ABEND                                            
037300                                                                          
037400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
037500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
037600     SKIP2                                                                
037700 01  FELTEXT.                                                             
037800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
037900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
038000     EJECT                                                                
038100*    --- PARAMETRAR TILL POSTSUM                                          
038200*                                                                         
038300*01  -COPY W0005   -PRE  POSTSUM-                                         
038400     EJECT                                                                
038500 01  W27147-AREA-START           PIC X(24)   VALUE                        
038600                                 'W27147-AREA-START  '.                   
038700     SKIP2                                                                
038800                                                                          
038900 01  W27147-AREA.                                                         
039000     03  SDC-UPPGIFTER.                                                   
039100         05  -COPY W27147                                                 
039200     EJECT                                                                
039300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
039400*                                                                         
039500     EJECT                                                                
039600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
039700     SKIP3                                                                
039800 01  NYCKLAR-TILL-DLI.                                                    
039900     03  W-IDARTNR-X.                                                     
040000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
040100     03  W-IDDC-X.                                                        
040200         05  W-IDDC              PIC S9(1)   VALUE ZERO COMP-3.           
040300     SKIP2                                                                
040400*    --- STATUS-KOD FRÅN IMS                                              
040500 01  STATUS-WS                   PIC XX.                                  
040600     88  SEGMENT-FINNS                       VALUE '  '.                  
040700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
040800     SKIP2                                                                
040900 01  GODK-STATUSKODER.                                                    
041000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041100     SKIP3                                                                
041200 01  SSA1                        PIC X(64).                               
041300 01  SSA2                        PIC X(64).                               
041400     EJECT                                                                
041500*    --- IMS FUNKTIONSKODER                                               
041600*01  -COPY W0003                                                          
041700     EJECT                                                                
041800*    ---  DLI INPUT-OUTPUT AREA                                           
041900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
042000     SKIP3                                                                
042100 01  DLI-IO-AREA.                                                         
042200     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
042300     SKIP3                                                                
042400     03  WDK701   REDEFINES IO-AREA.                                      
042500*        05  -COPY WDK701                                                 
042600     SKIP3                                                                
042700     03  WDK711   REDEFINES IO-AREA.                                      
042800*        05  -COPY WDK711                                                 
042900     EJECT                                                                
043000 LINKAGE SECTION.                                                         
043100                                                                          
043200     EJECT                                                                
043300*01  -COPY W0008  -PRE WDK7-                                              
043400     05  FILLER                  PIC X.                                   
043500     EJECT                                                                
043600 PROCEDURE DIVISION  USING WDK7-PCB.                                      
043700     ENTRY 'DLITCBL' USING WDK7-PCB.                                      
043800                                                                          
043900     PERFORM A-INIT                                                       
044000     PERFORM IMS-GET-WDK7                                                 
044100     PERFORM UNTIL SEGMENT-SLUT                                           
044200        EVALUATE WDK7-SEG-NAME-FB                                         
044300           WHEN 'WDK701  '                                                
044400              MOVE SART-IDARTNR TO WS-SPAR-IDARTNR                        
044500           WHEN 'WDK711  '                                                
044600              PERFORM B-BEHANDLA-FLYTTA-SKRIV-SDC                         
044700         END-EVALUATE                                                     
044800         PERFORM IMS-GET-WDK7                                             
044900     END-PERFORM                                                          
045000     PERFORM Z-FINIT                                                      
045100     MOVE ZERO TO RETURN-CODE                                             
045200     GOBACK                                                               
045300     .                                                                    
045400     EJECT                                                                
045600                                                                          
045700 A-INIT SECTION.                                                          
045900     OPEN OUTPUT LDC-W27147                                               
046000                 SDC-W27147                                               
046100                 NDC-W27147                                               
046200                 CHN-W27147                                               
048800                                                                          
048900     ACCEPT DAGENS-DATUM  FROM DATE                                       
049000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
049100     .                                                                    
049200     EJECT                                                                
049300                                                                          
049500 B-BEHANDLA-FLYTTA-SKRIV-SDC SECTION.                                     
049700     MOVE WS-SPAR-IDARTNR     TO SLAG-IDARTNR                             
049800     IF SLAG-KVEFRS IN WDK711 NOT NUMERIC                                 
049900       MOVE +0 TO SLAG-KVEFRS IN WDK711                                   
050000     END-IF                                                               
050100     MOVE CORR SLAG-WDK711    TO SLAG-W27147                              
050200                                                                          
050300     MOVE 1 TO IX                                                         
050400     PERFORM UNTIL IX > 12                                                
050500        MOVE SLAG-RESEASON IN SLAG-WDK711(IX) TO                          
050600             SLAG-RESEASON IN SLAG-W27147(IX)                             
050700        ADD 1 TO IX                                                       
050800     END-PERFORM                                                          
050900                                                                          
051000     MOVE SLAG-IDDC IN SLAG-WDK711    TO WS-IDDC                          
051100     EVALUATE TRUE                                                        
051200        WHEN LDC                                                          
051300           PERFORM S01-SKRIV-LDC-W27147                                   
051400        WHEN SDC                                                          
051500           PERFORM S02-SKRIV-SDC-W27147                                   
051600        WHEN NDC                                                          
051610           IF SLAG-IDDC-REF IN SLAG-WDK711 (1:1) = '7'                    
051620             PERFORM S04-SKRIV-CHN-W27147                                 
051630           ELSE                                                           
051631             IF SLAG-IDDC IN SLAG-WDK711 = '42'                           
051632               CONTINUE                                                   
051633             ELSE                                                         
051640               PERFORM S03-SKRIV-NDC-W27147                               
051650             END-IF                                                       
051660           END-IF                                                         
056800     END-EVALUATE                                                         
056900     .                                                                    
057000     EJECT                                                                
057100                                                                          
057300 Z-FINIT SECTION.                                                         
057400     CLOSE LDC-W27147                                                     
057500           SDC-W27147                                                     
057600           NDC-W27147                                                     
057700           CHN-W27147                                                     
060300     MOVE 'S' TO POSTSUM-OPKOD                                            
060400     CALL POSTSUM USING POSTSUM-PARM                                      
060500     .                                                                    
060600     EJECT                                                                
060700                                                                          
060900 S01-SKRIV-LDC-W27147 SECTION.                                            
061100     WRITE LDC-W27147-POST FROM W27147-AREA                               
061200                                                                          
061300     MOVE 'W27147' TO POSTSUM-FDNAMN                                      
061400     MOVE 'W27147D1' TO POSTSUM-DDNAMN2                                   
061500     CALL POSTSUM USING POSTSUM-PARM                                      
061600     .                                                                    
061700     EJECT                                                                
061800                                                                          
062000 S02-SKRIV-SDC-W27147 SECTION.                                            
062200     WRITE SDC-W27147-POST FROM W27147-AREA                               
062300                                                                          
062400     MOVE 'W27147' TO POSTSUM-FDNAMN                                      
062500     MOVE 'W27147D2' TO POSTSUM-DDNAMN2                                   
062600     CALL POSTSUM USING POSTSUM-PARM                                      
062700     .                                                                    
062800     EJECT                                                                
063000                                                                          
063100 S03-SKRIV-NDC-W27147 SECTION.                                            
063300     WRITE NDC-W27147-POST FROM W27147-AREA                               
063400                                                                          
063500     MOVE 'W27147' TO POSTSUM-FDNAMN                                      
063600     MOVE 'W27147D3' TO POSTSUM-DDNAMN2                                   
063700     CALL POSTSUM USING POSTSUM-PARM                                      
063800     .                                                                    
063900     EJECT                                                                
064000                                                                          
064100 S04-SKRIV-CHN-W27147 SECTION.                                            
064200     WRITE CHN-W27147-POST FROM W27147-AREA                               
064300                                                                          
064400     MOVE 'W27147' TO POSTSUM-FDNAMN                                      
064500     MOVE 'W27147D4' TO POSTSUM-DDNAMN2                                   
064600     CALL POSTSUM USING POSTSUM-PARM                                      
064700     .                                                                    
064800     EJECT                                                                
064900                                                                          
090300* --- IMS SEKTIONER ---                                                   
090400     SKIP3                                                                
090500     EJECT                                                                
090600 IMS-GET-WDK7   SECTION.                                                  
090800     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA                           
090900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
091000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
091100     PERFORM IMS-STATUSKONTROLL                                           
091200     .                                                                    
091300     SKIP3                                                                
091400 IMS-STATUSKONTROLL SECTION.                                              
091600     SET STATUS-IX TO 1                                                   
091700     SEARCH GODK-STATUS                                                   
091800       AT END                                                             
091900         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
092000         DISPLAY FELTEXT                                                  
092100         CALL FELLOG                                                      
092200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
092300         CONTINUE                                                         
092400     END-SEARCH                                                           
092500     .                                                                    
