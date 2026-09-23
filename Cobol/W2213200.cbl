000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2213200.                                                
000300 AUTHOR.         IDK, GÖTEBORG.                                           
000400 DATE-WRITTEN.   MARS 1979.                                               
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700*        PROGRAMMET SOM ÄR EN EXIT TILL HJÄLPPROGRAMMET IMS               
000800*        FAST SCAN UTILITY (FSU) LÄSER LEVERANTÖWREGISTRET                
000900*        (WDF1). FÖR VARJE LEVERANTÖR MED KONTROLLVECKA STÖRRE            
001000*        ÄN NOLL, SKRIVS EN POST (PTYP=002) PÅ UTFILEN.                   
001100*                                                                         
001200*        ÄNDRAT TILL SB/COBOL-II    890118           G.ERIKSSON           
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800*--------------------------------------- OUTPUT                           
001900     SKIP1                                                                
002000         SELECT UTFIL          ASSIGN TO UT-S-W22132D1.                   
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 FILE SECTION.                                                            
002400 FD  UTFIL                                                                
002500     RECORDING F                                                          
002600     BLOCK 0                                                              
002700     LABEL RECORD STANDARD.                                               
002800     SKIP1                                                                
002900*01  POST  -COPY W221002  -PRE UT-    -L.                                 
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300     SKIP3                                                                
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77      PROGRAM-NAMN    PIC X(8)    VALUE 'W2213200'.                    
003500                                                                          
003600 01  DYNAMISKA-SUBPGM.                                                    
003700     03  POSTSUM     PIC X(8)    VALUE 'POSTSUM '.                        
003800     03  CBLTDLI     PIC X(8)    VALUE 'CBLTDLI '.                        
003900     03  FELLOG      PIC X(8)    VALUE 'FELLOG'.                          
004000     EJECT                                                                
004100*--------------------------------------- PARAMETRAR TILL POSTSUM          
004200     SKIP1                                                                
004300*01  -COPY W0005  -PRE POSTSUM-                                           
004500     EJECT                                                                
004600*--------------------------------------- AREA FÖR POST PÅ UTFIL           
004700     SKIP1                                                                
004800*01  AREA  -COPY W221002  -PRE UT-                                        
005000     EJECT                                                                
005100 01  IMS-WS.                                                              
005200     03   FILLER     PIC X(8)  VALUE 'IMS-WS'.                            
005300*----------------------------------STATUSKODER FRÅN IMS                   
005400     03  STATUS-WS   PIC XX.                                              
005500         88  SEGMENT-FINNS       VALUE '  '.                              
005600         88  SEGMENT-SLUT        VALUE 'GB'.                              
005700                                                                          
005800     03  GODK-STATUSKODER.                                                
005900      05  GODK-STATUS  OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
006000                                                                          
006100     EJECT                                                                
006200*----------------------------------IMS-CALL FUNKTIONER                    
006300*01              -COPY W0003                                              
006500     EJECT                                                                
006600 01  DLI-IO-AREA             PIC X(200).                                  
006700*01  W221AB01  -COPY WDF101  -PRE LEV-  -RED DLI-IO-AREA                  
006900                                                                          
007000     EJECT                                                                
007100 LINKAGE SECTION.                                                         
007200*01    -COPY W0008         -PRE WDF1-                                     
007400          05  FILLER      PIC   XX.                                       
007500     EJECT                                                                
007600     SKIP3                                                                
007700 PROCEDURE DIVISION USING WDF1-PCB.                                       
007800     ENTRY 'CBLTDLI'  USING WDF1-PCB.                                     
007900     PERFORM A-INITIERA                                                   
008000     PERFORM IMS-GET-WDF1                                                 
008100     PERFORM UNTIL SEGMENT-SLUT                                           
008200         EVALUATE WDF1-SEG-NAME-FB                                        
008300              WHEN   'WDF101  '                                           
008400                 PERFORM B-SKAPA-UTPOST                                   
008500         END-EVALUATE                                                     
008600         PERFORM IMS-GET-WDF1                                             
008700     END-PERFORM                                                          
008800     PERFORM C-AVSLUTA                                                    
008900     MOVE ZERO TO RETURN-CODE                                             
009000     GOBACK                                                               
009100     .                                                                    
009200     EJECT                                                                
009300 A-INITIERA SECTION.                                                      
009400     SKIP1                                                                
009500     OPEN OUTPUT UTFIL                                                    
009600     MOVE SPACE              TO UT-AREA                                   
009700     MOVE '002'              TO UT-IDPTYP                                 
009800     MOVE 'W22132'           TO POSTSUM-PROGNAMN                          
009900     .                                                                    
010000     EJECT                                                                
010100 B-SKAPA-UTPOST SECTION.                                                  
010200     SKIP1                                                                
010300     MOVE 'IMS'              TO POSTSUM-FDNAMN                            
010400     MOVE 'WDF1'             TO POSTSUM-DDNAMN2                           
010500     MOVE 'AB01'             TO POSTSUM-TRANSTYP                          
010600     CALL POSTSUM USING POSTSUM-PARM                                      
010700     SKIP1                                                                
010710     IF LEV-LEV-IDLEVNR = '910'                                           
010720        CONTINUE                                                          
010730     ELSE                                                                 
010800     IF  LEV-LEV-IDLPKOLL GREATER ZERO                                    
010900         MOVE LEV-LEV-IDLEVNR   TO   UT-IDLEVNR                           
011000         MOVE LEV-LEV-IDLPKOLL  TO   UT-IDLPKOLL                          
011100         PERFORM S01-SKRIV-UTPOST                                         
011200     END-IF                                                               
011210     END-IF                                                               
011300     .                                                                    
011400     EJECT                                                                
011500 C-AVSLUTA SECTION.                                                       
011600     SKIP1                                                                
011700     CLOSE UTFIL                                                          
011800     MOVE 'S'                   TO POSTSUM-OPKOD                          
011900     CALL POSTSUM USING POSTSUM-PARM                                      
012000     .                                                                    
012100     EJECT                                                                
012200******************************************************************        
012300*                                                                *        
012400*        SKRIV EN POST PÅ UTFIL.                                 *        
012500*                                                                *        
012600******************************************************************        
012700     SKIP2                                                                
012800 S01-SKRIV-UTPOST SECTION.                                                
012900     SKIP1                                                                
013000     WRITE UT-POST           FROM UT-AREA                                 
013100     SKIP1                                                                
013200     MOVE 'UTFIL'            TO POSTSUM-FDNAMN                            
013300     MOVE 'W22132D1'         TO POSTSUM-DDNAMN2                           
013400     MOVE '002'              TO POSTSUM-TRANSTYP                          
013500     CALL POSTSUM USING POSTSUM-PARM                                      
013600     .                                                                    
013700     SKIP1                                                                
013800 IMS-GET-WDF1 SECTION.                                                    
013900     SKIP3                                                                
014000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014100     CALL CBLTDLI USING GN WDF1-PCB DLI-IO-AREA                           
014200     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
014300     PERFORM IMS-STATUSKONTROLL                                           
014400     .                                                                    
014500 IMS-STATUSKONTROLL SECTION.                                              
014600     SKIP3                                                                
014700     SET STATUS-IX TO 1                                                   
014800     SEARCH GODK-STATUS AT END CALL FELLOG                                
014900       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
015000       CONTINUE                                                           
015100     END-SEARCH                                                           
015200     .                                                                    
