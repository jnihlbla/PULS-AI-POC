000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2216800.                                                
000300 AUTHOR.         SURESH GUDIVADA.                                         
000400 DATE-WRITTEN.   03/11/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FEATURE:                                                             
000800*                                                                         
000900*        PROGRAM FOR POSTING PROCUREMENT SEGMENTATION CODE                
001000*        AND AUTO-ASSIGNMENT OF PROCSEGMCODE PER PART GLOBALLY            
001100*                                                                         
001200*                                                                         
001300                                                                          
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          ---                                                            
002200     SELECT W27177                     ASSIGN TO W22168D1.                
002300*          ---                                                            
002400     SELECT W22168                     ASSIGN TO W22168D2.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W27177                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  POST -COPY W27177 -PRE  W27177-  -L.                                 
003500     SKIP3                                                                
003600 FD  W22168                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST -COPY W22168 -PRE  UT-  -L.                                     
004100                                                                          
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W2216800'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000 77  W27177-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W27177                       VALUE 'J'.                   
005200                                                                          
005300 01  ARBETSAREOR.                                                         
005400     03 WS-DAGENS-TID            PIC 9(10)  VALUE ZERO.                   
005800                                                                          
005900**   03 WS-IDPROJ                PIC X(4).                                
006000**   03 WS-IDPROJ-REDEFINE       REDEFINES WS-IDPROJ.                     
006100**      05 WS-IDPROJ-2POS        PIC X(2).                                
006200**      05 WS-IDPROJ-FILLER      PIC X(2).                                
006300                                                                          
006400                                                                          
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600                                                                          
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STATUS          PIC X(2)    VALUE SPACE.                 
007000     03  FILLER                  PIC X       VALUE SPACE.                 
007100     03  FELTEXT-TEXT            PIC X(69)   VALUE SPACE.                 
007200                                                                          
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400*                                                                         
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     03  W221SEGM                PIC X(8)    VALUE 'W221SEGM'.            
008400                                                                          
008500     EJECT                                                                
008600 01  IN-AREA-START-W27177    PIC X(24)   VALUE                            
008700                                 'IN-AREA-START-W27177 '.                 
008800                                                                          
008900                                                                          
009000*01  AREA -COPY W27177     -PRE W27177-                                   
009100                                                                          
009200     EJECT                                                                
009300 01  UT-AREA-START           PIC X(24)   VALUE                            
009400                                 'UT-AREA-START  '.                       
009500                                                                          
009600                                                                          
009700*01  AREA -COPY W22168     -PRE UT-                                       
009800     EJECT                                                                
009900                                                                          
010000*    --- PARAMETRAR TILL W221SEGM                                         
010100*                                                                         
010200*01  -COPY W221SEGM                                                       
010300     EJECT                                                                
010400                                                                          
010500*    --- PARAMETRAR TILL POSTSUM                                          
010600*                                                                         
010700*01  -COPY W0005   -PRE  POSTSUM-                                         
010800     EJECT                                                                
010900                                                                          
011000*    --- WORK AREAS TO IMS-SECTIONS                                       
011100                                                                          
011200     SKIP3                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  NYCKLAR-TILL-DLI.                                                    
011600     03  W-IDDC-B6-X.                                                     
011700         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
011800                                                                          
011900*                                                                         
012000     SKIP2                                                                
012100*    --- STATUS-CODE FROM IMS                                             
012200 01  STATUS-WS                   PIC XX.                                  
012300     88  SEGMENT-FINNS                       VALUE '  '.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
012500     SKIP2                                                                
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(64).                               
013000 01  SSA2                        PIC X(64).                               
013100     EJECT                                                                
013200*    --- IMS FUNCTION CODES                                               
013300*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600                                                                          
014000     EJECT                                                                
014100                                                                          
014200 LINKAGE SECTION.                                                         
014800 01  SEGM-WDB6-PCB                 PIC X.                                 
014900 01  SEGM-WDL7-PCB                 PIC X.                                 
015000 01  SEGM-WDL8-PCB                 PIC X.                                 
015010 01  SEGM-WDD5-PCB                 PIC X.                                 
015020 01  SEGM-WDK7-PCB                 PIC X.                                 
015100     EJECT                                                                
015110                                                                          
015200 PROCEDURE DIVISION  USING SEGM-WDB6-PCB                                  
015300                           SEGM-WDL7-PCB                                  
015400                           SEGM-WDL8-PCB                                  
015500                           SEGM-WDD5-PCB                                  
015501                           SEGM-WDK7-PCB.                                 
015502                                                                          
015503     ENTRY 'DLITCBL' USING SEGM-WDB6-PCB                                  
015504                           SEGM-WDL7-PCB                                  
015505                           SEGM-WDL8-PCB                                  
015506                           SEGM-WDD5-PCB                                  
015507                           SEGM-WDK7-PCB.                                 
015800                                                                          
015900 MAIN SECTION.                                                            
016000                                                                          
016100     PERFORM A-INIT                                                       
016200                                                                          
016600     PERFORM S01-READ-W27177                                              
016700     PERFORM UNTIL END-OF-W27177                                          
016800       MOVE W27177-IDARTNR   TO UT-IDARTNR                                
016900       PERFORM B-SET-KDANSKSEG                                            
018400       PERFORM S03-WRITE-W22168                                           
018600       PERFORM S01-READ-W27177                                            
018700     END-PERFORM                                                          
018800                                                                          
018900     PERFORM Z-FINIT                                                      
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     .                                                                    
019500     EJECT                                                                
019510                                                                          
019600 A-INIT SECTION.                                                          
019800     OPEN INPUT  W27177                                                   
019900     OPEN OUTPUT W22168                                                   
020000                                                                          
020100     ACCEPT DAGENS-DATUM FROM DATE                                        
020200     ACCEPT WS-DAGENS-TID   FROM TIME                                     
020300     DISPLAY 'START TID : ' WS-DAGENS-TID                                 
020400                                                                          
020500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020600     INITIALIZE W27177-AREA                                               
020700                SEGM-W221SEGM                                             
020701                UT-AREA                                                   
020702     .                                                                    
020800     EJECT                                                                
020900                                                                          
021000 B-SET-KDANSKSEG SECTION.                                                 
021100***PREPARE-SEGM-LINK-AREA                                                 
037400     MOVE 001                 TO SEGM-KDCALL                              
037500     MOVE W27177-IDARTNR      TO SEGM-IDARTNR                             
037600     MOVE W27177-KDANSKSEG    TO SEGM-KDANSKSEG-IN                        
038300     MOVE SPACE               TO SEGM-IDDC                                
038301     MOVE SPACE               TO SEGM-IDREFTAB-IN                         
038302     MOVE W27177-KDPRODSL     TO SEGM-KDPRODSL                            
039000     MOVE W27177-FLBSNES      TO SEGM-FLBSNES                             
039001     MOVE W27177-KVEOP        TO SEGM-KVEOP                               
039002     MOVE W27177-KDVSOP       TO SEGM-KDVSOP                              
039003     MOVE W27177-TISOP        TO SEGM-TISOP                               
039004     MOVE W27177-TIURPROD     TO SEGM-TIURPROD                            
039005     MOVE W27177-KDFARLIG     TO SEGM-KDFARLIG                            
039006***                                                                       
039007     CALL W221SEGM USING SEGM-W221SEGM                                    
039008                         SEGM-WDB6-PCB                                    
039009                         SEGM-WDL7-PCB                                    
039010                         SEGM-WDL8-PCB                                    
039020                         SEGM-WDD5-PCB                                    
039021                         SEGM-WDK7-PCB                                    
039030     IF  SEGM-KDSVAR-OK                                                   
039040         IF SEGM-FLANSKSEG-CHANGED = JA                                   
039050            MOVE SEGM-KDANSKSEG       TO UT-KDANSKSEG                     
039060         ELSE                                                             
039070            MOVE SEGM-KDANSKSEG-IN    TO UT-KDANSKSEG                     
039080         END-IF                                                           
039090         MOVE SEGM-KDFGPRIO           TO UT-KDFGPRIO                      
039091         MOVE SEGM-FLANSKSEG-CHANGED  TO UT-FLANSKSEG-CHANGED             
039100     ELSE                                                                 
039200         DISPLAY 'W221SEGM-ERROR1:' SEGM-TEXT                             
039300         CALL FELLOG                                                      
039301     END-IF                                                               
039302     .                                                                    
039303     EJECT                                                                
039304                                                                          
039305 Z-FINIT SECTION.                                                         
039306     ACCEPT WS-DAGENS-TID   FROM TIME                                     
039307     DISPLAY 'SLUT  TID : ' WS-DAGENS-TID                                 
039308     CLOSE W27177                                                         
039309           W22168                                                         
039310     SKIP2                                                                
039311     MOVE 'S' TO POSTSUM-OPKOD                                            
039312     CALL POSTSUM USING POSTSUM-PARM                                      
039313     .                                                                    
039314     EJECT                                                                
039315                                                                          
039316 S01-READ-W27177  SECTION.                                                
039317     READ W27177    INTO W27177-AREA                                      
039318     AT END                                                               
039319        SET END-OF-W27177 TO TRUE                                         
039320     NOT AT END                                                           
039321        MOVE 'W27177 '       TO POSTSUM-FDNAMN                            
039322        MOVE 'W22168D1'      TO POSTSUM-DDNAMN2                           
039323        MOVE SPACE           TO POSTSUM-TRANSTYP                          
039324        CALL POSTSUM USING POSTSUM-PARM                                   
039325                                                                          
039326     END-READ                                                             
039327     .                                                                    
039328     EJECT                                                                
039329                                                                          
039330 S03-WRITE-W22168     SECTION.                                            
039331     WRITE UT-POST FROM UT-AREA                                           
039332     MOVE 'W22168 '          TO POSTSUM-FDNAMN                            
039333     MOVE 'W22168D2'         TO POSTSUM-DDNAMN2                           
039334     MOVE SPACE              TO POSTSUM-TRANSTYP                          
039335     CALL POSTSUM USING POSTSUM-PARM                                      
039336     .                                                                    
039337     EJECT                                                                
039338                                                                          
039339* --- IMS SECTIONS ---                                                    
039340     SKIP3                                                                
039341     EJECT                                                                
039342                                                                          
040300 IMS-STATUSKONTROLL SECTION.                                              
040500     SET STATUS-IX TO 1                                                   
040600     SEARCH GODK-STATUS                                                   
040700       AT END                                                             
040800         STRING 'UNAUTHORIZED RETURN CODE FROM IMS: ' STATUS-WS           
040900           DELIMITED BY SIZE INTO FELTEXT-TEXT                            
041000         DISPLAY FELTEXT                                                  
041100         CALL FELLOG                                                      
041200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041300         CONTINUE                                                         
041400     END-SEARCH                                                           
041500     .                                                                    
