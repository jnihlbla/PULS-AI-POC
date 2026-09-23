011100 ID DIVISION.                                                             
011200 PROGRAM-ID.     W0050900.                                                
011300 AUTHOR.         RICHARD.                                                 
011500 DATE-WRITTEN.   APRIL 89.                                                
011510 DATE-COMPILED.                                                           
011600                                                                          
011700*    FUNKTION.   PROGRAM FÖR ATT TESTA 32K INPUT FRÅN AS400.              
011800                                                                          
011900     SKIP3                                                                
012000 ENVIRONMENT DIVISION.                                                    
012100     SKIP3                                                                
012200 DATA DIVISION.                                                           
012300     EJECT                                                                
012400 WORKING-STORAGE SECTION.                                                 
012401                                                                          
012410*    -- CHECKED BY WY2000                                                 
012500 77    IDPGM                 PIC X(8)    VALUE 'W0050900'.                
012510 77    W-COMPILED            PIC X(16)   VALUE SPACE.                     
012520 77    FELTEXT               PIC X(80)   VALUE SPACE.                     
012600 77    JA                    PIC X       VALUE 'J'.                       
012700 77    NEJ                   PIC X       VALUE 'N'.                       
012800 77    W-INDX                PIC S9(9)   VALUE ZERO    COMP SYNC.         
012801     SKIP3                                                                
012810 01    MFS-RENSA-FAELT.                                                   
012820   03    FILLER              PIC 9(3)    VALUE 53      COMP-3.            
012900     SKIP3                                                                
013000 01    W-RAD.                                                             
013100   03    W-UTANT             PIC 9(4)    VALUE ZERO.                      
013200   03    FILLER              PIC X(6)    VALUE SPACE.                     
013210   03    FILLER              PIC X(83)   VALUE ZERO.                      
013300   03    FILLER              PIC X(7)    VALUE 'RADSLUT'.                 
013400     SKIP3                                                                
013500 01    DYNAMISKA-SUBPROGRAM.                                              
013600   03    CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
013700   03    FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
013800     EJECT                                                                
013900******************************************************************        
014000*                                                                         
014100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
014200*                                                                         
014300 01    FILLER                PIC X(16)   VALUE '     MFS-WS     '.        
014400     SKIP3                                                                
014500 01    MID.                                                               
014600   03    MID-INANT           PIC 9(4).                                    
014700   03    MID-UTANT           PIC 9(4).                                    
014800   03    FILLER              PIC X(100).                                  
014900     EJECT                                                                
015000 01    MSGAREA-TILL          PIC X(32000).                                
015100 01    FILLER REDEFINES MSGAREA-TILL.                                     
015200*  03    -COPY WMSGAREA                                                   
015400     05    MSG-MOD             REDEFINES MSG-AREA.                        
015500       07    MOD-IDTRANS     PIC X(4).                                    
015600       07    MOD-TEMFSFEL    PIC X(40).                                   
015700                                                                          
015800 01    FILLER REDEFINES MSGAREA-TILL.                                     
015900   03    FILLER              PIC X(48).                                   
016000   03    MOD-INANT-IN        PIC X(4).                                    
016010   03    MOD-INANT           PIC 9(4).                                    
016100   03    MOD-UTANT-IN        PIC X(4).                                    
016110   03    MOD-UTANT           PIC 9(4).                                    
016200   03    MOD-DATA.                                                        
016300     05    MOD-RAD OCCURS 40 PIC X(100).                                  
016400     EJECT                                                                
016500*01      -COPY WMSGSPAR                                                   
016700     EJECT                                                                
016800******************************************************************        
016900*                                                                         
017000*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017100*                                                                         
017200 01    IMS-WS.                                                            
017300   03    FILLER              PIC X(16)   VALUE 'IMS-WS     '.             
017400     SKIP3                                                                
017500   03    STATUS-WS           PIC XX.                                      
017600     88    SEGMENT-FINNS                 VALUE '  '.                      
017700     88    SEGMENT-SAKNAS                VALUE 'GE'.                      
017800     SKIP3                                                                
017900   03    GODK-STATUSKODER.                                                
018000     05    GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.              
018100     EJECT                                                                
018200*01    -COPY W0003                                                        
018400     EJECT                                                                
018500 LINKAGE SECTION.                                                         
018600*01    -COPY W0009     -PRE MSG-                                          
018800     EJECT                                                                
018900 PROCEDURE DIVISION USING MSG-PCB.                                        
019100 MAIN SECTION.                                                            
019110     ENTRY 'DLITCBL' USING MSG-PCB.                                       
019200                                                                          
019210     PERFORM IMS-GET-MSG                                                  
019300     IF SEGMENT-FINNS                                                     
019400       PERFORM A-INIT-SPARA-INPUT                                         
019500       IF MID-INANT NUMERIC                                               
019600         MOVE MID-INANT TO MOD-INANT                                      
019700       ELSE                                                               
019800         MOVE 9999 TO MOD-INANT                                           
019900       END-IF                                                             
020000       IF MID-UTANT NUMERIC                                               
020100         MOVE MID-UTANT TO MOD-UTANT                                      
020200       ELSE                                                               
020300         MOVE 9999 TO MOD-UTANT                                           
020400       END-IF                                                             
020500       MOVE +1 TO W-INDX                                                  
020600       PERFORM 40 TIMES                                                   
020700         MOVE W-INDX TO W-UTANT                                           
020800         MOVE W-RAD TO MOD-RAD (W-INDX)                                   
020900         ADD +1 TO W-INDX                                                 
021000       END-PERFORM                                                        
021100       IF MOD-UTANT = 9999                                                
021200         MOVE +1056 TO MSG-KVLL                                           
021300       ELSE                                                               
021400         MOVE +56 TO MSG-KVLL                                             
021500         ADD MOD-UTANT TO MSG-KVLL                                        
021600       END-IF                                                             
021700       PERFORM IMS-INSERT-MSG                                             
021800     END-IF                                                               
021900     MOVE ZERO TO RETURN-CODE                                             
022000     GOBACK                                                               
022100     .                                                                    
022200     EJECT                                                                
022300 A-INIT-SPARA-INPUT SECTION.                                              
022400     IF MSG-DUBBLA-TRANSKODER                                             
022500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID                          
022600       MOVE MSG-IDTRANS-2 TO MSG-SPAR-IDTRANS                             
022700       MOVE MSG-KDMFSFOR-2 TO MSG-SPAR-KDMFSFOR                           
022800     ELSE                                                                 
022900       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID                            
023000       MOVE MSG-IDTRANS-1 TO MSG-SPAR-IDTRANS                             
023100       MOVE MSG-KDMFSFOR-1 TO MSG-SPAR-KDMFSFOR                           
023200     END-IF                                                               
023300     MOVE LOW-VALUE TO MSG-AREA                                           
023400     MOVE 'W0O50901' TO MSG-SPAR-MODNAMN                                  
023500     MOVE '0509' TO MOD-IDTRANS                                           
023600     MOVE SPACE TO MOD-TEMFSFEL                                           
023610     MOVE MFS-RENSA-FAELT TO MOD-INANT-IN MOD-UTANT-IN                    
023700     .                                                                    
023800     EJECT                                                                
023900* IMS SEKTIONER                                                           
024000     SKIP3                                                                
024100 IMS-GET-MSG SECTION.                                                     
024200     MOVE '  QC' TO GODK-STATUSKODER                                      
024300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
024400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024500     PERFORM IMS-STATUSKONTROLL                                           
024600     .                                                                    
024700     SKIP3                                                                
024800 IMS-INSERT-MSG SECTION.                                                  
024900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
025000     MOVE SPACE TO GODK-STATUSKODER                                       
025100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MSG-SPAR-MODNAMN         
025200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025300     PERFORM IMS-STATUSKONTROLL                                           
025400     .                                                                    
025500     EJECT                                                                
025600 IMS-STATUSKONTROLL SECTION.                                              
025700     SET STATUS-IX TO 1                                                   
025800     SEARCH GODK-STATUS                                                   
025900       AT END                                                             
026000         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT                         
026010         CALL FELLOG                                                      
026100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026200         CONTINUE                                                         
026300     END-SEARCH                                                           
026400     .                                                                    
