001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W4264200.                                                
001200 AUTHOR.         KENT JEBSEN.                                             
001300 DATE-WRITTEN.   00/09/19.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        SKAPAR FIL FÖR FELLISTA - ARTIKLAR SOM SAKNAR INKÖPSPRIS.        
001710*        DATA KOMMER FRÅN KONTROLLRAPPORT-SYSTEMET.                       
001800*                                                                         
001910*        PROGRAMMET LÄSER      W6H7                                       
002000*                                                                         
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- KRL-SEGMENT                                                
003203     SELECT W42698                     ASSIGN TO W42642D1.                
003204     SKIP2                                                                
003205*          --- DATA TILL FELLISTA                                         
003210     SELECT W42642                     ASSIGN TO W42642D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W42698                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003806*01  -COPY W426PKR      -L.                                               
003807     SKIP3                                                                
003808 FD  W42642                                                               
003809     RECORDING       F                                                    
003810     BLOCK CONTAINS  0.                                                   
003811                                                                          
003820*01  POST -COPY W42642 -PRE  KRL-  -L.                                    
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W4264200'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004601                                                                          
004602 77  W42698-EOF-SW               PIC X       VALUE 'N'.                   
004610     88  END-OF-W42698                       VALUE 'J'.                   
004700     EJECT                                                                
004800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES DAGENS-DATUM.                                       
005000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005300     EJECT                                                                
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006000     SKIP2                                                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007001     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007201     EJECT                                                                
007202 01  KR-AREA-START               PIC X(24)   VALUE                        
007203                                 'KR-AREA-START  '.                       
007204     SKIP2                                                                
007205                                                                          
007206*01  AREA -COPY W426PKR   -PRE KR-                                        
007207     EJECT                                                                
007208 01  KRL-AREA-START              PIC X(24)   VALUE                        
007209                                 'KRL-AREA-START  '.                      
007210     SKIP2                                                                
007211                                                                          
007220*01  AREA -COPY W42642       -PRE KRL-                                    
007300     EJECT                                                                
007400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008001     03  W-IDKR-X.                                                        
008010         05  W-IDKR              PIC 9(5)   VALUE ZERO.                   
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(64).                               
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009901 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6H701'.                      
009902 01  DLI-IO-W6H701.                                                       
009910*    03  -COPY W6H701                                                     
010200     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010501                                                                          
010502*01  -COPY W0008  -PRE W6H7-                                              
010510     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010701 PROCEDURE DIVISION  USING W6H7-PCB.                                      
010702 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING W6H7-PCB.                                      
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011310     PERFORM S01-LAES-W42698                                              
011400     PERFORM UNTIL END-OF-W42698                                          
011600                                                                          
011610       MOVE KR-PKR-IDKR     TO W-IDKR                                     
011620       PERFORM IMS-GET-W6H701                                             
011630       MOVE KR-IDARTNR      TO KRL-IDARTNR                                
011640       MOVE KR-IDLEVNR      TO KRL-IDLEVNR                                
012000                                                                          
012100       PERFORM S11-SKRIV-W42642                                           
012101                                                                          
012110       PERFORM S01-LAES-W42698                                            
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
013201                                                                          
013210     OPEN INPUT  W42698                                                   
013301                                                                          
013310     OPEN OUTPUT W42642                                                   
013400                                                                          
013500     ACCEPT DAGENS-DATUM  FROM DATE                                       
013610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013800     .                                                                    
013900     EJECT                                                                
014000 Z-FINIT SECTION.                                                         
014101     CLOSE W42698                                                         
014110           W42642                                                         
014201     SKIP2                                                                
014202     MOVE 'S' TO POSTSUM-OPKOD                                            
014210     CALL POSTSUM USING POSTSUM-PARM                                      
014300     .                                                                    
014401     EJECT                                                                
014402 S01-LAES-W42698  SECTION.                                                
014403     READ W42698 INTO KR-AREA                                             
014404     AT END                                                               
014405        MOVE HIGH-VALUE TO KR-AREA                                        
014406        SET END-OF-W42698 TO TRUE                                         
014407                                                                          
014408     NOT AT END                                                           
014409        MOVE 'W42698' TO POSTSUM-FDNAMN                                   
014410        MOVE 'W42642D1' TO POSTSUM-DDNAMN2                                
014413        MOVE KR-PKR-IDPTYP TO POSTSUM-TRANSTYP                            
014414        CALL POSTSUM USING POSTSUM-PARM                                   
014415     END-READ                                                             
014420     .                                                                    
014501     EJECT                                                                
014502 S11-SKRIV-W42642 SECTION.                                                
014503                                                                          
014504     WRITE KRL-POST FROM KRL-AREA                                         
014505                                                                          
014506     MOVE KRL-IDPTYP TO POSTSUM-TRANSTYP                                  
014507     MOVE 'W42642' TO POSTSUM-FDNAMN                                      
014508     MOVE 'W42642D2' TO POSTSUM-DDNAMN2                                   
014509     CALL POSTSUM USING POSTSUM-PARM                                      
014510     .                                                                    
014700     EJECT                                                                
014800                                                                          
015400* --- IMS SEKTIONER ---                                                   
015500                                                                          
015602 IMS-GET-W6H701 SECTION.                                                  
015604     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
015605          DELIMITED BY SIZE INTO SSA1                                     
015606     MOVE '  GE' TO GODK-STATUSKODER                                      
015607     CALL CBLTDLI USING GU W6H7-PCB DLI-IO-W6H701 SSA1                    
015608     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
015609     PERFORM IMS-STATUSKONTROLL                                           
015610     .                                                                    
015700     EJECT                                                                
015800 IMS-STATUSKONTROLL SECTION.                                              
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
