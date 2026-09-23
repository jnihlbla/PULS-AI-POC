001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W3715200.                                                
001200 AUTHOR.         INGVAR SKJELBRED.                                        
001300 DATE-WRITTEN.   97/07/15.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        SKAPAR UTFIL FÖR ALLA BYTESRAPPORTER I STATUS = 2 OCH            
001800*        DATREG ÄLDRE ÄN 12 MÅNADER                                       
001900*                                                                         
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- UT FIL MED BYTESRAPPORTER I STATUS 2 ÄLDRE ÄN 3 MÅN        
003310     SELECT W37152                     ASSIGN TO W37152D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W37152                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  POST -COPY W37125 -PRE  UT-  -L.                                     
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004201                                                                          
004210*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W3715200'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  OK                          PIC X       VALUE ' '.                   
004800     EJECT                                                                
004900 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
005340 01  WS-DAREGDAT                 PIC 9(8)    VALUE ZERO.                  
005400     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006001     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006010     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006020     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
006100     SKIP2                                                                
006200*    --- PARAMETRAR TILL ABEND                                            
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
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
007120*01  -COPY WDAGAREA                                                       
007201     EJECT                                                                
007302 01  UT-AREA-START               PIC X(24)   VALUE                        
007303                                 'UT-AREA-START  '.                       
007304     SKIP2                                                                
007305                                                                          
007310*01  AREA -COPY W37125     -PRE UT-                                       
007400     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(64).                               
009200 01  SSA2                        PIC X(64).                               
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBYTF'.                      
009902 01  DLI-IO-WLBYTF.                                                       
009910*    03  -COPY WDM601  -PRE BYTF-                                         
010200     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010400                                                                          
010501     EJECT                                                                
010502*01  -COPY W0008  -PRE BYTF-                                              
010510     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010701 PROCEDURE DIVISION  USING BYTF-PCB.                                      
010702 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING BYTF-PCB.                                      
010800                                                                          
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011301     PERFORM IMS-GET-BYTF                                                 
011302     PERFORM UNTIL SEGMENT-SAKNAS                                         
011303       EVALUATE BYTF-SEG-NAME-FB                                          
011304           WHEN 'WDM601  '                                                
011305                 PERFORM B-SKAPA-UTFIL                                    
011306       END-EVALUATE                                                       
011307       PERFORM IMS-GET-BYTF                                               
011310     END-PERFORM                                                          
011400     PERFORM Z-FINIT                                                      
011500                                                                          
011600     MOVE ZERO TO RETURN-CODE                                             
011700     GOBACK                                                               
011800     .                                                                    
011900     EJECT                                                                
012000 A-INIT SECTION.                                                          
012201                                                                          
012210     OPEN OUTPUT W37152                                                   
012300                                                                          
012510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012511                                                                          
012512     ACCEPT DAG-TIAAMMDD-TOM FROM DATE                                    
012513     MOVE 275    TO DAG-KVKALDAG                                          
012514     MOVE 003    TO DAG-KDCALL                                            
012515                                                                          
012516     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
012517                                                                          
012518     IF DAG-KDSVAR = OK                                                   
012519       MOVE DAG-TIAAMMDD-FOM TO DAGENS-DATUM (3:6)                        
012520       MOVE DAG-TISEKEL-FOM  TO DAGENS-DATUM (1:2)                        
012530       DISPLAY '*** RENSNINGSDATUM FÖR WDM6 ****** '                      
012540       DISPLAY 'RENSNINGSDATUM ' DAGENS-DATUM                             
012550     ELSE                                                                 
012560       MOVE 'FEL I DATUMKONVERTERING' TO   FELTEXT-STR                    
012570       PERFORM S99-ABEND                                                  
012580     END-IF                                                               
012700     .                                                                    
012800     EJECT                                                                
012810 B-SKAPA-UTFIL SECTION.                                                   
012811                                                                          
012813     MOVE BYTF-RAPP-DAREGDAT   TO WS-DAREGDAT                             
012827                                                                          
012829     IF WS-DAREGDAT > DAGENS-DATUM                                        
012830        CONTINUE                                                          
012831     ELSE                                                                 
012832        IF BYTF-RAPP-KDBYTSTA-RAPP = '2'                                  
012833           MOVE BYTF-RAPP-IDDISTR    TO UT-IDDISTR                        
012834           MOVE BYTF-RAPP-IDBYTRAP   TO UT-IDBYTRAP                       
012835           PERFORM S11-SKRIV-W37152                                       
012836        END-IF                                                            
012837     END-IF                                                               
012838                                                                          
012839     .                                                                    
012840     EJECT                                                                
012900 Z-FINIT SECTION.                                                         
013010     CLOSE W37152                                                         
013101     SKIP2                                                                
013102     MOVE 'S' TO POSTSUM-OPKOD                                            
013110     CALL POSTSUM USING POSTSUM-PARM                                      
013200     .                                                                    
013401     EJECT                                                                
013402 S11-SKRIV-W37152 SECTION.                                                
013403                                                                          
013404     WRITE UT-POST FROM UT-AREA                                           
013405                                                                          
013407     MOVE 'W37152' TO POSTSUM-FDNAMN                                      
013408     MOVE 'W37152D1' TO POSTSUM-DDNAMN2                                   
013409     CALL POSTSUM USING POSTSUM-PARM                                      
013410     .                                                                    
013600     EJECT                                                                
013700 S99-ABEND SECTION.                                                       
013800                                                                          
013901     SKIP2                                                                
013902     MOVE 'S' TO POSTSUM-OPKOD                                            
013910     CALL POSTSUM USING POSTSUM-PARM                                      
014000     CALL ABEND USING RKOD-ABEND                                          
014100     .                                                                    
014200     EJECT                                                                
014300* --- IMS SEKTIONER ---                                                   
014400     SKIP3                                                                
014501     EJECT                                                                
014502 IMS-GET-BYTF   SECTION.                                                  
014503                                                                          
014504     CALL CBLTDLI USING GN BYTF-PCB DLI-IO-WLBYTF                         
014505     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
014506     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014507     PERFORM IMS-STATUSKONTROLL                                           
014510     .                                                                    
014600     EJECT                                                                
014700 IMS-STATUSKONTROLL SECTION.                                              
014800                                                                          
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
