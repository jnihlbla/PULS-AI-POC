000010     EJECT                                                                
000020 S90-ROLLBACK SECTION.                                                    
000030     SKIP2                                                                
000040                                                                          
000050     EXEC SQL                                                             
000060       ROLLBACK                                                           
000070     END-EXEC                                                             
000080     .                                                                    
000090     EJECT                                                                
000100*----------------------------------------------------------------*        
000110*---- KONTROLL AV SQLCODE       **** COBOL II ****            ---*        
000120*----  C ANROPAS I PROGRAMMET:  PERFORM S95-CONTROL-SQLCODE   ---*        
000130*----    INGA INITIERINGAR AV AREOR BEHÖVS.                   ---*        
000140*----                                                         ---*        
000150*---- OM SQLCODE HITTAS I TABELLEN MED GODK-SQLCODER :        ---*        
000160*----                                                         ---*        
000170*----    KAN DU VÄLJA FÖLJANDE GENOM ATT ÄNDRA I TABELLENS    ---*        
000180*----    DEFAULT-VALUES:                                      ---*        
000190*----     - DISPLAYA DSNTIAR-FELTEXT  Y ELLER N               ---*        
000200*----     - ZFINIT                    Y ELLER N               ---*        
000210*----     - GÖR ROLLBACK              Y ELLER N               ---*        
000220*----                                                         ---*        
000230*---- OM SQLCODE INTE FINNS I TABELLEN HÄNDER FÖLJANDE:       ---*        
000240*----                                                         ---*        
000250*---- 1. DSNTIAR-FELTEXT DISPLAYAS                            ---*        
000260*---- 2. ROLLBACK UTFÖRS                                      ---*        
000270*---- 3. ZFINIT                                               ---*        
000280*----------------------------------------------------------------*        
000290 S95-CONTROL-SQLCODE SECTION.                                             
000300     SKIP2                                                                
000310                                                                          
000320     SET GODK-SQLCODE-INDEX TO +1                                         
000330     SEARCH GODK-SQLCODER                                                 
000340         AT END                                                           
000350            PERFORM S97-SQLCODE-TEXT-FINIT                                
000360         WHEN GODK-SQLCODE(GODK-SQLCODE-INDEX) = SQLCODE                  
000370            PERFORM S98-ERROR-MESSAGE                                     
000380     END-SEARCH                                                           
000390     .                                                                    
000400     EJECT                                                                
000410*----------------------------------------------------------------*        
000420 S97-SQLCODE-TEXT-FINIT SECTION.                                          
000430     SKIP2                                                                
000440     PERFORM S99-DISPLAY-DSNTIAR-TEXT                                     
000450     PERFORM S90-ROLLBACK                                                 
000460     DISPLAY '--------- ROLLBACK PERFORMED ------------'                  
000470     MOVE RCODE-20 TO RCODE                                               
000480     PERFORM Z-FINIT                                                      
000490     GOBACK                                                               
000500     .                                                                    
000510     EJECT                                                                
000520*----------------------------------------------------------------*        
000530 S98-ERROR-MESSAGE SECTION.                                               
000540     SKIP2                                                                
000550     IF GODK-SQLCODE-DISPTX(GODK-SQLCODE-INDEX) = 'Y'                     
000560         PERFORM S99-DISPLAY-DSNTIAR-TEXT                                 
000570     END-IF                                                               
000580                                                                          
000590     IF GODK-SQLCODE-ROLBCK(GODK-SQLCODE-INDEX) = 'Y'                     
000600         PERFORM S90-ROLLBACK                                             
000610     END-IF                                                               
000620                                                                          
000630     IF GODK-SQLCODE-ZFINIT(GODK-SQLCODE-INDEX) = 'Y'                     
000640         MOVE RCODE-20 TO RCODE                                           
000650         PERFORM Z-FINIT                                                  
000660         GOBACK                                                           
000670     END-IF                                                               
000680     .                                                                    
000690     EJECT                                                                
000700*----------------------------------------------------------------*        
000710*---- DISPLAY AV FÖRKLARANDE TEXT TILL SQL-CODES ----------------*        
000720*---- DENNA SECTION KAN ANROPAS SEPARAT NÄR SOM HELST -----------*        
000730*----------------------------------------------------------------*        
000740 S99-DISPLAY-DSNTIAR-TEXT SECTION.                                        
000750     SKIP2                                                                
000760     PERFORM S99A-CALL-DSNTIAR                                            
000770                                                                          
000780     DISPLAY '--- SEVERE ERROR IN SECTION '                               
000790     ABEND-SECTION '---'                                                  
000800     DISPLAY '---DB2 ERRORTEXT: '                                         
000810     DISPLAY ' '                                                          
000820                                                                          
000830     PERFORM S99B-DISPLAY-ERROR-TEXT-RAD                                  
000840         VARYING ERROR-INDEX FROM +1 BY +1                                
000850         UNTIL ERROR-INDEX > 12                                           
000860         OR  ERROR-TEXT (ERROR-INDEX) = SPACE                             
000870     .                                                                    
000880     EJECT                                                                
000890*----------------------------------------------------------------*        
000900 S99A-CALL-DSNTIAR SECTION.                                               
000910     SKIP2                                                                
000920                                                                          
000930     MOVE SPACE TO ERROR-TEXT(1) ERROR-TEXT(2) ERROR-TEXT(3)              
000940                   ERROR-TEXT(4) ERROR-TEXT(5) ERROR-TEXT(6)              
000950                   ERROR-TEXT(7) ERROR-TEXT(8) ERROR-TEXT(9)              
000960                   ERROR-TEXT(10) ERROR-TEXT(11) ERROR-TEXT(12)           
000970                                                                          
000980     CALL S98-DSNTIAR USING SQLCA ERROR-MESSAGE ERROR-TEXT-LEN            
000990     END-CALL                                                             
001000     .                                                                    
001010     SKIP2                                                                
001020*----------------------------------------------------------------*        
001030 S99B-DISPLAY-ERROR-TEXT-RAD SECTION.                                     
001040     SKIP2                                                                
001050     DISPLAY ERROR-TEXT (ERROR-INDEX)                                     
001060     .                                                                    
001070*** END COPY VDB2PS95C2  LENGTH=0     OLD LENGTH=                         
