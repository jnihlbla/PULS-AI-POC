001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W4637800.                                                
001200 AUTHOR.         BO HAMMARIN.                                             
001300 DATE-WRITTEN.   JULI-99.                                                 
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        PROGRAMMET LÄSER                                                 
001701*        1 DISTRIBUTIONSLOGG (WDR6)                                       
001703*                                                                         
001704*        OCH SKAPAR                                                       
001800*        1 RENSNINGSFIL                                                   
001810*        2 FIL FÖR UPPFÖLJNING AV DDGS-HÄNDELSER                          
001900*                                                                         
002010*        PROGRAMMET LÄSER WLFILA MED SB (WDR6)                            
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     EJECT                                                                
002710                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003305*          --- UTFIL RENSNING                                             
003306     SELECT W46378                     ASSIGN TO W46378D1.                
003307     SKIP2                                                                
003308*          --- UTFIL UPPFÖLJNING                                          
003309     SELECT W46379                     ASSIGN TO W46378D2.                
003500     EJECT                                                                
003510                                                                          
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W46378                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003906*01  POST -COPY WDR601 -PRE  UT1-  -L.                                    
003908                                                                          
003909 FD  W46379                                                               
003910     RECORDING       F                                                    
003911     BLOCK CONTAINS  0.                                                   
003913*01  POST -COPY W46341 -PRE  UT2-  -L.                                    
004094     EJECT                                                                
004095                                                                          
004100 WORKING-STORAGE SECTION.                                                 
004201                                                                          
004210*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W4637800'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  W46378-EOF-SW               PIC X       VALUE 'N'.                   
004700     88  END-OF-W46378                       VALUE 'J'.                   
004810                                                                          
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005420     EJECT                                                                
005430                                                                          
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006300                                                                          
006310*    --- PARAMETRAR TILL ABEND                                            
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102                                                                          
007103*    --- PARAMETRAR TILL POSTSUM                                          
007104*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302                                                                          
007310 01  UT1-AREA-START              PIC X(24)   VALUE                        
007311                                 'UT1-AREA-START  '.                      
007314*01  AREA -COPY WDR601     -PRE UT1-                                      
007315*    05   -COPY W46341     -PRE UT1- -RED UT1-FIL-WDR601-DATA             
007316     EJECT                                                                
007317                                                                          
007318 01  UT2-AREA-START              PIC X(24)   VALUE                        
007319                                 'UT2-AREA-START  '.                      
007320     SKIP2                                                                
007321*01  AREA -COPY W46341     -PRE UT2-                                      
007325     EJECT                                                                
007326                                                                          
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900                                                                          
008000 01  NYCKLAR-TILL-DLI.                                                    
008101     03  W-WDR601KY-X.                                                    
008110         05  W-WDR601KY          PIC X(27)    VALUE SPACE.                
008200                                                                          
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008700                                                                          
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(64).                               
009200 01  SSA2                        PIC X(64).                               
009310                                                                          
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009700                                                                          
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLFILA'.                      
009902 01  DLI-IO-WLFILA.                                                       
009910*    03  -COPY WDR601                                                     
009911*    05   -COPY W46341     -RED FIL-WDR601-DATA                           
010200     EJECT                                                                
010210                                                                          
010300 LINKAGE SECTION.                                                         
010502*01  -COPY W0008  -PRE FILA-                                              
010510     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010700                                                                          
010701 PROCEDURE DIVISION  USING FILA-PCB.                                      
010703 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING FILA-PCB.                                      
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011301     PERFORM IMS-GET-FILA                                                 
011302     PERFORM UNTIL SEGMENT-SLUT                                           
011340       IF FIL-CT-IDSYSTEM = 'W463' AND                                    
011341         (FIL-CT-IDPTYP   = 'VIA' OR 'CHG')                               
011345          PERFORM B-FLYTTA-POST-TILL-UT1-UT2                              
011349       END-IF                                                             
011350       PERFORM IMS-GET-FILA                                               
011360     END-PERFORM                                                          
011361                                                                          
011420     PERFORM Z-FINIT                                                      
011500                                                                          
011600     MOVE ZERO TO RETURN-CODE                                             
011700     GOBACK                                                               
011800     .                                                                    
011900     EJECT                                                                
012201                                                                          
012202 A-INIT SECTION.                                                          
012204     OPEN OUTPUT W46378                                                   
012210                 W46379                                                   
012300                                                                          
012400     ACCEPT DAGENS-DATUM  FROM DATE                                       
012510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012700     .                                                                    
012800     EJECT                                                                
012901                                                                          
012902 B-FLYTTA-POST-TILL-UT1-UT2 SECTION.                                      
012903*                                                                         
012904*---FLYTTAR ALLA POSTER TILL UT1- OCH UT2-AREA                            
012905*---SKRIVER SEDAN POSTER PÅ W46378 OCH W46379                             
012906*                                                                         
012908     MOVE DLI-IO-WLFILA   TO UT1-AREA                                     
012909     MOVE FIL-WDR601-DATA TO UT2-AREA                                     
012910                                                                          
012918     PERFORM S10-SKRIV-W46378                                             
012919     PERFORM S11-SKRIV-W46379                                             
012921     .                                                                    
012922     EJECT                                                                
012923                                                                          
013180 Z-FINIT SECTION.                                                         
013181     CLOSE W46378                                                         
013182           W46379                                                         
013185     SKIP2                                                                
013186     MOVE 'S' TO POSTSUM-OPKOD                                            
013190     CALL POSTSUM USING POSTSUM-PARM                                      
013200     .                                                                    
013401     EJECT                                                                
013403                                                                          
013430 S10-SKRIV-W46378 SECTION.                                                
013431     WRITE UT1-POST  FROM UT1-AREA                                        
013432                                                                          
013433     MOVE 'UT1-'     TO POSTSUM-TRANSTYP                                  
013434     MOVE 'W46378'   TO POSTSUM-FDNAMN                                    
013435     MOVE 'W46378D1' TO POSTSUM-DDNAMN2                                   
013436     CALL POSTSUM USING POSTSUM-PARM                                      
013437     .                                                                    
013438                                                                          
013439 S11-SKRIV-W46379 SECTION.                                                
013441     WRITE UT2-POST  FROM UT2-AREA                                        
013442                                                                          
013443     MOVE 'UT2-'     TO POSTSUM-TRANSTYP                                  
013444     MOVE 'W46379'   TO POSTSUM-FDNAMN                                    
013445     MOVE 'W46379D2' TO POSTSUM-DDNAMN2                                   
013446     CALL POSTSUM USING POSTSUM-PARM                                      
013447     .                                                                    
013600     EJECT                                                                
013610                                                                          
013700*S99-ABEND SECTION.                                                       
013800*                                                                         
013901*    SKIP2                                                                
013902*    MOVE 'S' TO POSTSUM-OPKOD                                            
013910*    CALL POSTSUM USING POSTSUM-PARM                                      
014000*    CALL ABEND USING RKOD-ABEND                                          
014100*    .                                                                    
014200*    EJECT                                                                
014300* --- IMS SEKTIONER ---                                                   
014502     EJECT                                                                
014503                                                                          
014504 IMS-GET-FILA   SECTION.                                                  
014506     CALL CBLTDLI USING GN FILA-PCB DLI-IO-WLFILA                         
014507     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
014508     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014509     PERFORM IMS-STATUSKONTROLL                                           
014510     .                                                                    
014800                                                                          
014810 IMS-STATUSKONTROLL SECTION.                                              
014900     SET STATUS-IX TO 1                                                   
015000     SEARCH GODK-STATUS                                                   
015100       AT END                                                             
015200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015300           DELIMITED BY SIZE INTO FELTEXT                                 
015400         DISPLAY FELTEXT                                                  
015500         CALL FELLOG                                                      
015600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015700         CONTINUE                                                         
015800     END-SEARCH                                                           
015900     .                                                                    
