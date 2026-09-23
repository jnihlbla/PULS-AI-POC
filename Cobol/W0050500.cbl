000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W0050500.                                                
001000 AUTHOR.         RICHARD.                                                 
001100 DATE-WRITTEN.   FEB. 93.                                                 
001200 DATE-COMPILED.                                                           
001300                                                                          
001400*     FUNKTION.   TP-PROGRAM FÖR PARTS MENYHANTERING.                     
001420*                 PROGRAMMET LIGGER 'LOCAL' I V2                          
001430*                 FÖR ACF2 KONTROLL VID P-TO-P-SW.                        
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP3                                                                
001900 DATA DIVISION.                                                           
002000     EJECT                                                                
002100 WORKING-STORAGE SECTION.                                                 
002101                                                                          
002110*    -- CHECKED BY WY2000                                                 
002200 77    IDPGM                 PIC X(8)    VALUE 'W0050500'.                
002210 77    W-COMPILED            PIC X(16)   VALUE SPACE.                     
002220 77    FELTEXT               PIC X(80)   VALUE SPACE.                     
002300 77    JA                    PIC X       VALUE 'J'.                       
002400 77    NEJ                   PIC X       VALUE 'N'.                       
004100     SKIP3                                                                
004200 01    DYNAMISKA-SUBPROGRAM.                                              
004300   03    CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
004400   03    FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
004500     EJECT                                                                
071600******************************************************************        
071700*                                                                         
071800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
071900*                                                                         
072000 01    FILLER                PIC X(16)   VALUE 'MFS-WS'.                  
072100     SKIP3                                                                
072200 01    MID-W0I50501.                                                      
073900   03    MID-DATA.                                                        
074000     05    MID-KVLL          PIC S9(4)   COMP.                            
074001     05    MID-Z1-Z2         PIC X(2).                                    
074010     05    FILLER            PIC X(13).                                   
074100     05    MID-KEY           PIC X(10).                                   
075700     EJECT                                                                
075800*01    -COPY WMSGSNUF                                                     
075900     EJECT                                                                
076700   03    MOD-MENY                REDEFINES MSG-AREA.                      
076710     05    MOD-IDTRANS       PIC X(4).                                    
076900     05    MOD-KDMFSFOR      PIC X(1).                                    
077000     05    MOD-TEMFSINF      PIC X(55).                                   
077500     EJECT                                                                
077600*01    -COPY WMSGSPAR                                                     
077700     EJECT                                                                
077800******************************************************************        
077900*                                                                         
078000*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
078100*                                                                         
078200 01    IMS-WS.                                                            
078300   03    FILLER              PIC X(16)   VALUE ' IMS-WS     '.            
078400     SKIP3                                                                
078500   03    STATUS-WS           PIC XX.                                      
078600     88    STATUS-OK                     VALUE '  '.                      
078700     88    SEGMENT-FINNS                 VALUE '  '.                      
078900     88    TRANSKOD-FEL                  VALUE 'A1'.                      
079000     88    SECURITY-FEL                  VALUE 'A4'.                      
079200     SKIP3                                                                
079300   03    GODK-STATUSKODER.                                                
079400     05    GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.              
079500     EJECT                                                                
079600*01    -COPY W0003                                                        
079700     EJECT                                                                
079800 LINKAGE SECTION.                                                         
079900*01    -COPY W0009     -PRE MSG-                                          
080000     SKIP2                                                                
080100*01    -COPY W0009     -PRE ALT-                                          
080200     EJECT                                                                
080300 PROCEDURE DIVISION USING MSG-PCB ALT-PCB.                                
080400 MAIN SECTION.                                                            
080500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB.                               
080600                                                                          
080900     PERFORM IMS-GET-MSG                                                  
081000     IF SEGMENT-FINNS                                                     
082213       PERFORM A-INIT-SPARA-INPUT                                         
082220       PERFORM B-CHANGE                                                   
082500     END-IF                                                               
082510                                                                          
082600     MOVE ZERO TO RETURN-CODE                                             
082700     GOBACK                                                               
082800     .                                                                    
082900     EJECT                                                                
083000 A-INIT-SPARA-INPUT SECTION.                                              
083100                                                                          
085500     MOVE WHEN-COMPILED TO W-COMPILED                                     
085501                                                                          
085510     MOVE MSG-INDATA   TO MID-W0I50501                                    
085520     MOVE MSG-IDTRANS  TO MSG-SPAR-IDTRANS                                
085530     MOVE MSG-KDMFSFOR TO MSG-SPAR-KDMFSFOR                               
085540     MOVE MSG-KDTRTYP  TO MSG-SPAR-KDTRTYP                                
085550     MOVE SPACE        TO MSG-SPAR-IDPFK                                  
091300     .                                                                    
091400     EJECT                                                                
091500 B-CHANGE SECTION.                                                        
091600                                                                          
105001     MOVE LOW-VALUE TO MID-Z1-Z2                                          
105002     IF MID-KVLL > +27                                                    
105003       MOVE +27 TO MID-KVLL                                               
105004     END-IF                                                               
105005     MOVE MID-DATA TO MSG-IO-AREA-SNUF                                    
105006                                                                          
105010     PERFORM IMS-CHANGE-ALTMSG                                            
105100     IF STATUS-OK                                                         
105200       PERFORM IMS-INSERT-ALTMSG                                          
105300     ELSE                                                                 
105400       MOVE MSG-SPAR-IDTRANS  TO MOD-IDTRANS                              
105500       MOVE MSG-SPAR-KDMFSFOR TO MOD-KDMFSFOR                             
106300       IF SECURITY-FEL                                                    
106400         STRING ' SECURITY VIOLATION '                                    
106500                STATUS-WS ' ' MID-DATA                                    
106600                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
106700       ELSE                                                               
106800         STRING ' WRONG PICTURE '                                         
106900                STATUS-WS ' ' MID-DATA                                    
107000                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
107100       END-IF                                                             
107200       MOVE 'W0O50401' TO MSG-SPAR-MODNAMN                                
107300       MOVE +64 TO MSG-KVLL                                               
107400       PERFORM IMS-INSERT-MSG                                             
107500     END-IF                                                               
110000     .                                                                    
121900     EJECT                                                                
122000* IMS SEKTIONER                                                           
122100     SKIP2                                                                
122200 IMS-GET-MSG SECTION.                                                     
122300     MOVE '  QCCF' TO GODK-STATUSKODER                                    
122400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA-SNUF                       
122500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
122600     PERFORM IMS-STATUSKONTROLL                                           
122700     .                                                                    
122800     SKIP2                                                                
122900 IMS-INSERT-MSG SECTION.                                                  
123000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
123100     MOVE SPACE TO GODK-STATUSKODER                                       
123200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA-SNUF                     
123210                                     MSG-SPAR-MODNAMN                     
123300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
123400     PERFORM IMS-STATUSKONTROLL                                           
123500     .                                                                    
123600     SKIP2                                                                
123700 IMS-CHANGE-ALTMSG SECTION.                                               
123800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
123900     MOVE '  A1A4' TO GODK-STATUSKODER                                    
124000     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS                          
124100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
124200     PERFORM IMS-STATUSKONTROLL                                           
124300     .                                                                    
124400     SKIP2                                                                
124500 IMS-INSERT-ALTMSG SECTION.                                               
124600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
124700     MOVE SPACE TO GODK-STATUSKODER                                       
124800     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA-SNUF                     
124900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
125000     PERFORM IMS-STATUSKONTROLL                                           
125100     .                                                                    
125200     EJECT                                                                
125300 IMS-STATUSKONTROLL SECTION.                                              
125400                                                                          
125500     SET STATUS-IX TO 1                                                   
125600     SEARCH GODK-STATUS                                                   
125700       AT END                                                             
125800         MOVE 'FEL STATUSKOD FRÅN IMS ' TO FELTEXT                        
125810         CALL FELLOG                                                      
125900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
126000         CONTINUE                                                         
126100     END-SEARCH                                                           
126200     .                                                                    
