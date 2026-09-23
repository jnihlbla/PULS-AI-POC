000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300                                                                          
000400 ID  DIVISION.                                                            
000500                                                                          
000600 PROGRAM-ID.    W9102800.                                                 
000700*              PROGRAM CONVERTED BY                                       
000800*              COBOL CONVERSION AID PO 5785-ABJ                           
000900*              CONVERSION DATE 05/25/91 17:37:35.                         
001000*AUTHOR.        CHRISTINA BRUHN                                           
001100*DATE-WRITTEN.  OKT  1986.                                                
001200                                                                          
001300*REMARKS.                                                                 
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        PROGRAMMET LÄSER WDN6  MED SB.                                   
001800*        SKAPAR EN FIL MED SAMTLIGA ARTNR OCH DERAS EMBLEM.               
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*--- UTFIL:                                                               
002700                                                                          
002800     SELECT W91028                       ASSIGN TO UT-S-W91028D1.         
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W91028                                                               
003500     RECORDING       V                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  UTPOST -COPY W91028L1   -L.                                          
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004701                                                                          
004710*    -- CHECKED BY WY2000                                                 
004800 77  PROGRAM-NAMN                PIC X(6) VALUE 'W91028'.                 
005000*                                      GENERERAT PROGRAM-NAMN.            
005100 01  DYNAMISKA-SUBPROGRAM.                                                
005200                                                                          
005300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
005700                                                                          
005800                                                                          
005900     EJECT                                                                
006000*01  -COPY W0005      -PRE POSTSUM-                                       
006200 01  UT-TRANSID.                                                          
006300     03  FILLER                  PIC X(6)  VALUE 'W91028'.                
006400     03  FILLER                  PIC X(8)  VALUE 'W91028D1'.              
006500     03  FILLER                  PIC X(4)  VALUE 'POST'.                  
006600     EJECT                                                                
006700     EJECT                                                                
006800 01  FILLER                      PIC X(8)    VALUE 'UT-AREOR'.            
006900                                                                          
007000*01  POST -COPY W91028L1   -PRE UT-                                       
007200     EJECT                                                                
007300 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
007400                                                                          
007500 01  IMS-WS.                                                              
007600                                                                          
007700     03  STATUS-WS               PIC X(2).                                
007800        88  SEGMENT-SLUT                     VALUE 'GB'.                  
008100                                                                          
008200     03  GODK-STATUSKODER.                                                
008300         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
008400                                                                          
008500*01  -COPY W0003                                                          
008700     EJECT                                                                
008800 01  DLI-IO-AREA.                                                         
008900     03  IO-AREA              PIC X(40).                                  
009000     SKIP3                                                                
009100*    03  FILLER -COPY WDN601 -PRE KAT-  -RED IO-AREA                      
009300     SKIP3                                                                
009400*    03  FILLER -COPY WDN611 -PRE KAT-  -RED IO-AREA                      
009600     SKIP3                                                                
009700 LINKAGE SECTION.                                                         
009800     SKIP3                                                                
009900*01  -COPY W0008   -PRE WDN6-                                             
010100         05  FILLER           PIC X(1).                                   
010200     EJECT                                                                
010300 PROCEDURE DIVISION USING  WDN6-PCB.                                      
010400     ENTRY 'DLITCBL' USING WDN6-PCB.                                      
010500                                                                          
010600     PERFORM A-INIT                                                       
010700                                                                          
010800     PERFORM IMS-GET-WDN6                                                 
010900                                                                          
011000     PERFORM UNTIL SEGMENT-SLUT                                           
011200       EVALUATE WDN6-SEG-NAME-FB                                          
011300       WHEN 'WDN601'                                                      
011400         MOVE KAT-MAST-IDARTNR  TO UT-IDARTNR                             
011500       WHEN 'WDN611'                                                      
011600         MOVE KAT-KAT-BEEMBLEM  TO UT-BEEMBLEM                            
011610         MOVE KAT-KAT-KDFORDON  TO UT-KDFORDON                            
011700         PERFORM S01-SKRIV-W91028                                         
011800       END-EVALUATE                                                       
011900       PERFORM IMS-GET-WDN6                                               
012000     END-PERFORM                                                          
012100     PERFORM Z-FINIT                                                      
012200     MOVE ZERO TO RETURN-CODE                                             
012300     GOBACK                                                               
012400     .                                                                    
012500     EJECT                                                                
012600 A-INIT SECTION.                                                          
012700                                                                          
012800     OPEN OUTPUT W91028                                                   
012900                                                                          
013000     MOVE '028'        TO UT-IDPTYP                                       
013100     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
013200     .                                                                    
013300                                                                          
013400     EJECT                                                                
013500 Z-FINIT  SECTION.                                                        
013600                                                                          
013700     CLOSE W91028                                                         
013800                                                                          
013900     MOVE 'S' TO POSTSUM-OPKOD                                            
014000     CALL POSTSUM USING POSTSUM-PARM                                      
014100     .                                                                    
014200     EJECT                                                                
014300 S01-SKRIV-W91028     SECTION.                                            
014400                                                                          
014500     WRITE UTPOST FROM UT-POST                                            
014600                                                                          
014700     MOVE UT-TRANSID      TO POSTSUM-TRANSID                              
014800     CALL POSTSUM USING POSTSUM-PARM                                      
014900     .                                                                    
015000     EJECT                                                                
015100*         * I M S  S E C T I O N                                          
015200                                                                          
015300 IMS-GET-WDN6         SECTION.                                            
015400                                                                          
015500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015600     CALL CBLTDLI USING GN WDN6-PCB IO-AREA                               
015700     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
015800     PERFORM IMS-STATUSKONTROLL                                           
015900     .                                                                    
016000     SKIP3                                                                
016100 IMS-STATUSKONTROLL   SECTION.                                            
016200                                                                          
016300     SET STATUS-IX TO 1                                                   
016400     SEARCH GODK-STATUS                                                   
016500          AT END                                                          
016600            CALL FELLOG                                                   
016700          WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                        
016800            CONTINUE                                                      
016900     END-SEARCH                                                           
017100     .                                                                    
