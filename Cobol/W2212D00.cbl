000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2212D00.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   16/11/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER WDD4 MED SB. SKAPAR EN FIL MED ALLA 220         
000900*        LARM SOM SKALL KONTROLLERAS MOT LEVERANTÖRSREGISTRET FÖR         
001000*        ATT EVENTUELLT SKAPA AUTOMATISKA MAIL,BILD 2119.                 
001100*        SE BILD 2172 FÖR CDC OCH 2472 FÖR KINA SOM VISAR ALLA            
001200*        220-LARM.                                                        
001210*                                                                         
001300*        PROGRAMMET LÄSER      WDD4                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900******************************************************************        
002000* 2016-11-04  E'TRACKER 10214419 PRE ADVISEMENT MISSING, SEND E-MA        
002100*                                BY AUTOMATIC.                            
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- LARM 220-POSTER CDC/KINA                                   
003200     SELECT W2212D01                   ASSIGN TO W2212DD1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W2212D01                                                             
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  POST -COPY W2212D -PRE  UT-  -L.                                     
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600 77  IDPGM                       PIC X(8)    VALUE 'W2212D00'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
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
006100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
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
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL POSTSUM                                          
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700 01  UT-AREA-START               PIC X(24)   VALUE                        
007800                                 'UT-AREA-START  '.                       
007900     SKIP2                                                                
008000                                                                          
008100*01  AREA -COPY W2212D     -PRE UT-                                       
008200     EJECT                                                                
008300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008400*                                                                         
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008700     SKIP3                                                                
008800 01  NYCKLAR-TILL-DLI.                                                    
008900     03  W-WDD401KY-X.                                                    
009000         05  W-WDD401KY          PIC S9(13)   VALUE ZERO COMP-3.          
009100     SKIP2                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FINNS                       VALUE '  '.                  
009500     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
009600     SKIP2                                                                
009700 01  GODK-STATUSKODER.                                                    
009800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP3                                                                
010000 01  SSA1                        PIC X(64).                               
010100 01  SSA2                        PIC X(64).                               
010200     EJECT                                                                
010300*    --- IMS FUNKTIONSKODER                                               
010400*01  -COPY W0003                                                          
010500     EJECT                                                                
010600*    ---  DLI INPUT-OUTPUT AREA                                           
010700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD401'.                      
010800 01  DLI-IO-WDD401.                                                       
010900*    03  -COPY WDD401                                                     
011000     EJECT                                                                
011100 LINKAGE SECTION.                                                         
011200                                                                          
011300                                                                          
011400*01  -COPY W0008  -PRE WDD4-                                              
011500     05  FILLER                  PIC X.                                   
011600     EJECT                                                                
011700 PROCEDURE DIVISION  USING WDD4-PCB.                                      
011800 MAIN SECTION.                                                            
011900     ENTRY 'DLITCBL' USING WDD4-PCB.                                      
012000                                                                          
012100                                                                          
012200     PERFORM A-INIT                                                       
012300                                                                          
012400     PERFORM IMS-GN-WDD4                                                  
012500     PERFORM UNTIL SEGMENT-SAKNAS                                         
012600                                                                          
012700       IF LAK-KDLARM = 220                                                
012800         PERFORM B-BEHANDLA-LARM                                          
012900         PERFORM S11-SKRIV-W2212D01                                       
013000       END-IF                                                             
013100       PERFORM IMS-GN-WDD4                                                
013200     END-PERFORM                                                          
013300                                                                          
013400     PERFORM Z-FINIT                                                      
013500                                                                          
013600     MOVE ZERO TO RETURN-CODE                                             
013700     GOBACK                                                               
013800     .                                                                    
013900     EJECT                                                                
014000 A-INIT SECTION.                                                          
014100                                                                          
014200     OPEN OUTPUT W2212D01                                                 
014300                                                                          
014400     ACCEPT DAGENS-DATUM  FROM DATE                                       
014500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014600     .                                                                    
014700     EJECT                                                                
014800 B-BEHANDLA-LARM  SECTION.                                                
014900                                                                          
015000     MOVE LAK-IDLEVNR     TO UT-IDLEVNR                                   
015100     MOVE LAK-IDDC        TO UT-IDDC                                      
015200     MOVE LAK-IDANSK      TO UT-IDANSK                                    
015300     MOVE LAK-IDARTNR     TO UT-IDARTNR                                   
015400     MOVE LAK-TIAAMMDD    TO UT-TIAAMMDD                                  
015500     MOVE LAK-KVAVROP     TO UT-KVAVROP                                   
015600     MOVE LAK-KVAVIS      TO UT-KVAVIS                                    
015610     MOVE LAK-DAREGDAT-9KOMPL  TO UT-DAREGDAT-9KOMPL                      
015620     MOVE LAK-TIKLOCK-9KOMPL   TO UT-TIKLOCK-9KOMPL                       
015630     MOVE 'PADE'          TO UT-KDMAIL                                    
015700     .                                                                    
015800     EJECT                                                                
015900 Z-FINIT SECTION.                                                         
016000     CLOSE W2212D01                                                       
016100     SKIP2                                                                
016200     MOVE 'S' TO POSTSUM-OPKOD                                            
016300     CALL POSTSUM USING POSTSUM-PARM                                      
016400     .                                                                    
016500     EJECT                                                                
016600 S11-SKRIV-W2212D01 SECTION.                                              
016700                                                                          
016800     WRITE UT-POST FROM UT-AREA                                           
016900                                                                          
017000     MOVE '220 '     TO POSTSUM-TRANSTYP                                  
017100     MOVE 'W2212D'   TO POSTSUM-FDNAMN                                    
017200     MOVE 'W2212DD1' TO POSTSUM-DDNAMN2                                   
017300     CALL POSTSUM USING POSTSUM-PARM                                      
017400     .                                                                    
017500     EJECT                                                                
017600 S99-ABEND SECTION.                                                       
017700                                                                          
017800     SKIP2                                                                
017900     MOVE 'S' TO POSTSUM-OPKOD                                            
018000     CALL POSTSUM USING POSTSUM-PARM                                      
018100     CALL ABEND USING RKOD-ABEND                                          
018200     .                                                                    
018300     EJECT                                                                
018400* --- IMS SEKTIONER ---                                                   
018500                                                                          
018600                                                                          
018700 IMS-GN-WDD4   SECTION.                                                   
018800                                                                          
018900     CALL CBLTDLI USING GN WDD4-PCB DLI-IO-WDD401                         
019000     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
019100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
019200     PERFORM IMS-STATUSKONTROLL                                           
019300     .                                                                    
019400     EJECT                                                                
019500 IMS-STATUSKONTROLL SECTION.                                              
019600                                                                          
019700     SET STATUS-IX TO 1                                                   
019800     SEARCH GODK-STATUS                                                   
019900       AT END                                                             
020000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
020100           DELIMITED BY SIZE INTO FELTEXT                                 
020200         DISPLAY FELTEXT                                                  
020300         CALL FELLOG                                                      
020400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
020500         CONTINUE                                                         
020600     END-SEARCH                                                           
020700     .                                                                    
