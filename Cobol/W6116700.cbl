000100 ID  DIVISION.                                                            
000200                                                                          
000300 PROGRAM-ID.    W6116700.                                                 
000400 AUTHOR.        ANDERS HENRIKSSON.                                        
000500 DATE-WRITTEN.  MARS 2007.                                                
000600                                                                          
000700* FUNKTION: LÄSER ARTIKELREGISTER WDK6 MED SB                             
001200*                                                                         
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*--- URVALSFIL :                                                          
002500                                                                          
002600     SELECT W61167                       ASSIGN TO UT-S-W61167D1.         
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W61167                                                               
003700     RECORDING  F                                                         
003800     BLOCK CONTAINS 0.                                                    
003900                                                                          
004000*01  POST -COPY W61167 -PRE  SORTWS- -L.                                  
004100     SKIP3                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600 77  IDPGM                       PIC X(8)    VALUE 'W6116700'.            
005700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(3) VALUE +16.                     
005910 77  HUVUD-LEV-SW                PIC X       VALUE 'J'.                   
005920     88  NYASTE-HUVUD-LEV                    VALUE 'J'.                   
005930     88  GAMMAL-HUVUD-LEV                    VALUE 'N'.                   
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
006300 77  EOF                         PIC X       VALUE 'N'.                   
006400                                                                          
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007400                                                                          
008200*01  -COPY W0005      -PRE POSTSUM-                                       
008300     EJECT                                                                
008400                                                                          
009500 01  FILLER                      PIC X(8)    VALUE 'SOUTAREA'.            
009510 01  SOUT-AREA.                                                           
009600*    03   -COPY W61167 -PRE SOUT-.                                        
009700                                                                          
010000 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
010200 01  IMS-WS.                                                              
010400     03  STATUS-WS               PIC X(2).                                
010500        88  SEGMENT-SLUT                     VALUE 'GB'.                  
010600        88  SEGMENT-FINNS                    VALUE '  ' 'GA' 'GK'.        
010700        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
010800                                                                          
010900     03  GODK-STATUSKODER.                                                
011000         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
011100                                                                          
011200*01  -COPY W0003                                                          
011300     EJECT                                                                
011400 01  DLI-IO-AREA.                                                         
011500     03  IO-AREA              PIC X(900).                                 
011600     SKIP3                                                                
011700*    03  FILLER -COPY WDK601 -RED IO-AREA                                 
011800     EJECT                                                                
011900*    03  FILLER -COPY WDK611 -RED IO-AREA                                 
012000     EJECT                                                                
012010*    03  FILLER -COPY WDK621 -RED IO-AREA                                 
012020     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200     SKIP3                                                                
012300*01  -COPY W0008   -PRE WDK6-                                             
012400         05  FILLER           PIC X(1).                                   
012500     EJECT                                                                
012600 PROCEDURE DIVISION USING  WDK6-PCB.                                      
012700     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
012800                                                                          
012900     PERFORM A-INIT                                                       
013000                                                                          
014310     PERFORM B-SORT-INPUT                                                 
014400     PERFORM Z-FINIT                                                      
014500     MOVE ZERO TO RETURN-CODE                                             
014600     GOBACK                                                               
014800     .                                                                    
014900     EJECT                                                                
014910                                                                          
015000 A-INIT SECTION.                                                          
015300     OPEN OUTPUT W61167                                                   
016200                                                                          
016300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016400     .                                                                    
016500     EJECT                                                                
016510                                                                          
016600 B-SORT-INPUT SECTION.                                                    
016800     PERFORM IMS-GET-WDK6                                                 
016810                          DISPLAY 'WDK6   '                               
016900                                                                          
017000     PERFORM UNTIL SEGMENT-SLUT                                           
017100        EVALUATE WDK6-SEG-NAME-FB                                         
017200           WHEN 'WDK601 ' MOVE ART-IDARTNR  TO SOUT-IDARTNR               
017300                          MOVE ART-IDLEVNR  TO SOUT-IDLEVNR               
017400                          MOVE JA           TO HUVUD-LEV-SW               
017600                                                                          
017700           WHEN 'WDK611 ' MOVE CLAG-BEFT    TO SOUT-BEFT                  
017900                                                                          
017910           WHEN 'WDK621 ' IF PRL-IDLEVNR = SOUT-IDLEVNR                   
017912                            IF NYASTE-HUVUD-LEV                           
017920                              PERFORM BA-SKRIV-POST                       
017921                              PERFORM S04-SKRIV-UTPOST                    
017922                              MOVE NEJ TO HUVUD-LEV-SW                    
017924                            END-IF                                        
017930                          END-IF                                          
018000        END-EVALUATE                                                      
018100        PERFORM IMS-GET-WDK6                                              
018200     END-PERFORM                                                          
018300     .                                                                    
018400     EJECT                                                                
018410                                                                          
018500 BA-SKRIV-POST SECTION.                                                   
018800     MOVE PRL-DAPRLIST-9KOMPL             TO SOUT-DAPRLIST-9KOMPL         
018900     MOVE PRL-KDFPKPRI                    TO SOUT-KDFPKPRI                
023500     .                                                                    
023600     EJECT                                                                
023700                                                                          
026300 Z-FINIT  SECTION.                                                        
026500     MOVE 'S' TO POSTSUM-OPKOD                                            
026600     CALL POSTSUM USING POSTSUM-PARM                                      
026700                                                                          
026900     CLOSE W61167                                                         
027000     .                                                                    
027100     EJECT                                                                
027110                                                                          
030400 S04-SKRIV-UTPOST SECTION.                                                
030600     WRITE SORTWS-POST FROM SOUT-AREA                                     
030700                                                                          
030800     MOVE '21'       TO POSTSUM-TRANSTYP                                  
030900     MOVE 'W61167'   TO POSTSUM-FDNAMN                                    
031000     MOVE 'W61167D1' TO POSTSUM-DDNAMN2                                   
031100     CALL POSTSUM USING POSTSUM-PARM                                      
031200     .                                                                    
031300     EJECT                                                                
031310                                                                          
031400 S99-ABEND SECTION.                                                       
031500     MOVE 'S' TO POSTSUM-OPKOD                                            
031600     CALL POSTSUM USING POSTSUM-PARM                                      
031700                                                                          
031800     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
031900     .                                                                    
032000     EJECT                                                                
032200                                                                          
032210*         * I M S  S E C T I O N                                          
032300 IMS-GET-WDK6         SECTION.                                            
032500     MOVE '  GAGKGBGAGK' TO GODK-STATUSKODER                              
032600     CALL CBLTDLI USING GN WDK6-PCB IO-AREA                               
032700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032800     PERFORM IMS-STATUSKONTROLL                                           
032900     .                                                                    
033000     SKIP3                                                                
033010                                                                          
033100 IMS-STATUSKONTROLL   SECTION.                                            
033300     SET STATUS-IX TO 1                                                   
033400     SEARCH GODK-STATUS                                                   
033500       AT END CALL FELLOG                                                 
033600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
033700     END-SEARCH                                                           
033800     .                                                                    
