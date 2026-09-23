000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4762300.                                                
000400*AUTHOR.         STINA MOGREN.                                            
000500*DATE-WRITTEN.   03/02/14.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER NER LOGGTRANSAR FRÅN SHIPN.AVSLUT TILL SEKV.FIL.           
001010*        FÖR DANZAS NORGE                                                 
001100*                                                                         
001110*        SKAPAR DESSUTOM EN FIL FÖR ATT RENSA WDR7                        
001120*                                                                         
001200*        PROGRAMMET LÄSER      WDR7 MED SB.                               
001300*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- TRANSAR                                                    
003100     SELECT W47601                     ASSIGN TO W47623D1.                
003200     SKIP2                                                                
003600*          --- WDR7 TRANSAR SOM SKALL RENSAS                              
003700     SELECT W47604                     ASSIGN TO W47623D2.                
003710     SKIP2                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W47601                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600     SKIP2                                                                
004700*01  POST -COPY W4762501  -PRE  UT-   -L.                                 
004800     SKIP3                                                                
005700 FD  W47604                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000     SKIP2                                                                
006100*01  POST -COPY WDR701    -PRE  RENS-  -L.                                
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006400     SKIP2                                                                
006401                                                                          
006410*    -- CHECKED BY WY2000                                                 
006500 77  IDPGM                       PIC X(8)    VALUE 'W4762300'.            
006600 77  JA                          PIC X       VALUE 'J'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800     EJECT                                                                
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007500     SKIP2                                                                
007600*    --- PARAMETRAR TILL ABEND                                            
007700                                                                          
007800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008000     SKIP2                                                                
008100 01  FELTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400     EJECT                                                                
008401 01  W-URVAL                     PIC X(200).                              
008402 01  FILLER                      REDEFINES W-URVAL.                       
008403     03  W-URV-IDSHIPM           PIC 9(7).                                
008410                                                                          
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008900 01  W4762301-AREA-START           PIC X(24)   VALUE                      
009000                                 'W4762301-AREA-START  '.                 
009100     SKIP2                                                                
009200*01  AREA   -COPY W4762501  -PRE UT-                                      
009300     EJECT                                                                
009310                                                                          
009810                                                                          
010400 01  WDR701-RENS-START           PIC X(24)   VALUE                        
010500                                 'WDR701-RENS-START  '.                   
010600     SKIP2                                                                
010700*01  AREA -COPY WDR701      -PRE RENS-                                    
010800     EJECT                                                                
010900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011000*                                                                         
011100     SKIP2                                                                
011200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011300     SKIP3                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012210     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDR701'.         
013500     SKIP2                                                                
013600 01  DLI-IO-WDR701.                                                       
013800     SKIP3                                                                
013900*    03  -COPY WDR701                                                     
014100     EJECT                                                                
014800 LINKAGE SECTION.                                                         
014900     SKIP2                                                                
015000*01  -COPY W0008  -PRE WDR7-                                              
015100     05  FILLER                  PIC X.                                   
015200     EJECT                                                                
015300 PROCEDURE DIVISION  USING WDR7-PCB.                                      
015400     ENTRY 'DLITCBL' USING WDR7-PCB.                                      
015500                                                                          
015600     SKIP2                                                                
015700     PERFORM A-INIT                                                       
015800                                                                          
015900     PERFORM IMS-GN-WDR7-FIL                                              
016000     PERFORM UNTIL (NOT SEGMENT-FINNS)                                    
016100                                                                          
016200       IF  FIL-CT-IDSYSTEM = 'W476'                                       
016210         AND FIL-IDPGM = 'W4063200'                                       
016300         EVALUATE FIL-CT-IDPTYP                                           
016400           WHEN 'DAN'                                                     
016500             PERFORM C-SKAPA-POST                                         
016600             PERFORM E-SKAPA-RENS-POST                                    
017300           WHEN OTHER                                                     
017400             CONTINUE                                                     
017500         END-EVALUATE                                                     
017600       ELSE                                                               
017700         CONTINUE                                                         
017800       END-IF                                                             
017900                                                                          
018000       PERFORM IMS-GN-WDR7-FIL                                            
018100     END-PERFORM                                                          
018200                                                                          
018300     PERFORM Z-FINIT                                                      
018400                                                                          
018500     MOVE ZERO TO RETURN-CODE                                             
018600     GOBACK                                                               
018700     .                                                                    
018800     EJECT                                                                
018900 A-INIT SECTION.                                                          
019000                                                                          
019100     OPEN OUTPUT W47601                                                   
019300                 W47604                                                   
019400                                                                          
019500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019600     .                                                                    
019700     EJECT                                                                
023300 C-SKAPA-POST SECTION.                                                    
023400                                                                          
023700     MOVE FIL-WDR701-DATA        TO UT-SHIP-W4762501                      
023800     PERFORM S11-SKRIV-W47601                                             
023900     .                                                                    
024000     EJECT                                                                
035000 E-SKAPA-RENS-POST SECTION.                                               
035100                                                                          
035200     MOVE FIL-IDPGM              TO RENS-FIL-IDPGM                        
035300     MOVE FIL-TIREGDAT           TO RENS-FIL-TIREGDAT                     
035400     MOVE FIL-TIKLOCK            TO RENS-FIL-TIKLOCK                      
035500     MOVE FIL-IDSEKVNR           TO RENS-FIL-IDSEKVNR                     
035600     MOVE FIL-CT-IDSYSTEM        TO RENS-FIL-CT-IDSYSTEM                  
035700     MOVE FIL-CT-IDPTYP          TO RENS-FIL-CT-IDPTYP                    
035800     MOVE FIL-CT-IDVTYP          TO RENS-FIL-CT-IDVTYP                    
035810     MOVE FIL-WDR701-DATA        TO RENS-FIL-WDR701-DATA                  
035900     PERFORM S14-SKRIV-W47604                                             
036000     .                                                                    
036100     EJECT                                                                
036336 Z-FINIT SECTION.                                                         
036340     CLOSE W47601                                                         
036500           W47604                                                         
036600     SKIP2                                                                
036700     MOVE 'S' TO POSTSUM-OPKOD                                            
036800     CALL POSTSUM USING POSTSUM-PARM                                      
036900     .                                                                    
037000     EJECT                                                                
037100 S11-SKRIV-W47601 SECTION.                                                
037200     SKIP2                                                                
037300     WRITE UT-POST   FROM UT-AREA                                         
037400                                                                          
037500     MOVE 'DAN'            TO POSTSUM-TRANSTYP                            
037600     MOVE 'W47601'         TO POSTSUM-FDNAMN                              
037700     MOVE 'W47623D1'       TO POSTSUM-DDNAMN2                             
037800     CALL POSTSUM USING POSTSUM-PARM                                      
037900     .                                                                    
038000     EJECT                                                                
040100 S14-SKRIV-W47604 SECTION.                                                
040200     SKIP2                                                                
040300     WRITE RENS-POST     FROM RENS-FIL-WDR701                             
040400                                                                          
040500     MOVE SPACE           TO POSTSUM-TRANSTYP                             
040600     MOVE 'W47604'        TO POSTSUM-FDNAMN                               
040700     MOVE 'W47623D2'      TO POSTSUM-DDNAMN2                              
040800     CALL POSTSUM USING POSTSUM-PARM                                      
040900     .                                                                    
041000     EJECT                                                                
041110* --- IMS SEKTIONER ---                                                   
041200     SKIP3                                                                
041300 IMS-GN-WDR7-FIL SECTION.                                                 
041500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
041600     CALL CBLTDLI USING GN WDR7-PCB DLI-IO-WDR701                         
041700     MOVE WDR7-STATUS-CODE TO STATUS-WS                                   
041800     PERFORM IMS-STATUSKONTROLL                                           
041900     .                                                                    
042000     SKIP3                                                                
042100 IMS-STATUSKONTROLL SECTION.                                              
042200     SKIP2                                                                
042300     SET STATUS-IX TO 1                                                   
042400     SEARCH GODK-STATUS                                                   
042500       AT END                                                             
042600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
042700           DELIMITED BY SIZE INTO FELTEXT-STR                             
042800         DISPLAY FELTEXT                                                  
042900         CALL FELLOG                                                      
043000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
043100         CONTINUE                                                         
043200     END-SEARCH                                                           
043300     .                                                                    
