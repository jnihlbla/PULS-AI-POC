001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4798400.                                                
001300*AUTHOR.         BOO HAMMARIN CGL.                                        
001400*DATE-WRITTEN.   92/03/10.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        LÄSER NER WDK901 OCH SELEKTERAR RAKT AV TILL EN FIL              
002000*                            SB                                           
002010*        DÄR VARJE ARTIKEL GENERERAR EN POST PER CLAGER                   
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
003402*          --- UTFIL                                                      
003410     SELECT W47984                     ASSIGN TO W47984D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W47984                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005     SKIP2                                                                
004010 01  W47984-POST                 PIC X(49).                               
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W4798400'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004900     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
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
007301     EJECT                                                                
007306 01  UT-AREA-START               PIC X(24)   VALUE                        
007307                                 'UT-AREA-START  '.                       
007308     SKIP2                                                                
007309                                                                          
007310*01  AREA -COPY W47984     -PRE  CDC-                                     
007311     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  BASEN-SLUT                          VALUE 'GB'.                  
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
009900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010000     SKIP3                                                                
010100 01  DLI-IO-AREA.                                                         
010200     03  IO-AREA                 PIC X(100)  VALUE SPACE.                 
010310     03  WLARTM01                REDEFINES IO-AREA.                       
010320*        05  -COPY WDK901 -PRE ARTM-                                      
010600     EJECT                                                                
010700 LINKAGE SECTION.                                                         
010800                                                                          
010901     EJECT                                                                
010902*01  -COPY W0008  -PRE ARTM-                                              
010910     05  FILLER                  PIC X.                                   
011000     EJECT                                                                
011101 PROCEDURE DIVISION  USING ARTM-PCB.                                      
011200                                                                          
011500     PERFORM A-INIT                                                       
011700     PERFORM IMS-GET-ARTM                                                 
011701                                                                          
011710     PERFORM UNTIL BASEN-SLUT                                             
011800                                                                          
011910        PERFORM S10-BYGG-W47984                                           
012000        PERFORM S11-SKRIV-W47984                                          
012010        PERFORM IMS-GET-ARTM                                              
012300                                                                          
012500     END-PERFORM                                                          
012700                                                                          
012810     PERFORM Z-FINIT                                                      
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013601                                                                          
013610     OPEN OUTPUT W47984                                                   
013700                                                                          
013910     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014100     .                                                                    
014200     EJECT                                                                
014300 Z-FINIT SECTION.                                                         
014410                                                                          
014420     CLOSE W47984                                                         
014501                                                                          
014502     MOVE 'S' TO POSTSUM-OPKOD                                            
014510     CALL POSTSUM USING POSTSUM-PARM                                      
014600     .                                                                    
014801     EJECT                                                                
014802 S10-BYGG-W47984 SECTION.                                                 
014803                                                                          
014807     MOVE ARTM-ART-WDK901           TO CDC-W47984                         
014825     .                                                                    
014826     EJECT                                                                
014827 S11-SKRIV-W47984 SECTION.                                                
014828                                                                          
014829     WRITE W47984-POST FROM CDC-AREA                                      
014830                                                                          
014831     MOVE 'UT'      TO POSTSUM-TRANSTYP                                   
014832     MOVE 'W47984' TO POSTSUM-FDNAMN                                      
014833     MOVE 'W47984D1' TO POSTSUM-DDNAMN2                                   
014834     CALL POSTSUM USING POSTSUM-PARM                                      
014835                                                                          
014850     .                                                                    
015000     EJECT                                                                
015100 S99-ABEND SECTION.                                                       
015301                                                                          
015302     MOVE 'S' TO POSTSUM-OPKOD                                            
015310     CALL POSTSUM USING POSTSUM-PARM                                      
015400     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
015500     .                                                                    
015600     EJECT                                                                
015902 IMS-GET-ARTM   SECTION.                                                  
015903                                                                          
015904     MOVE '  GAGKGB'       TO GODK-STATUSKODER                            
015905     CALL CBLTDLI USING GN ARTM-PCB DLI-IO-AREA                           
015906     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
015907     PERFORM IMS-STATUSKONTROLL                                           
015910     .                                                                    
016000     SKIP2                                                                
016100 IMS-STATUSKONTROLL SECTION.                                              
016200                                                                          
016300     SET STATUS-IX TO 1                                                   
016400     SEARCH GODK-STATUS                                                   
016500       AT END                                                             
016600         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT-STR                     
016700         DISPLAY FELTEXT                                                  
016800         CALL FELLOG                                                      
016900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017000         CONTINUE                                                         
017100     END-SEARCH                                                           
017200     .                                                                    
