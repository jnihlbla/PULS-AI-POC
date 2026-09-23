000010 01  PRT-W006PRT.                                                         
000020*                                 PARAMETRAR TILL W006PRT  FÖR            
000030*                                 FÖR ATT ÖVERSÄTTA EN LOGISK             
000040*                                 PRINTERIDENTITET TILL                   
000050*                                 IMS-LTERM ELLER MVS-NODE.               
000060*                                 EXEMPEL PÅ ANROP:                       
000070*                                                                         
000080*                                 1. VILKEN LTERM?                        
000090*                                 MOVE 001 TO PRT-KDCALL                  
000100*                                 MOVE "US1     " TO PRT-IDPRTLST         
000110*                                 CALL W006PRT USING PRT-W006PRT          
000120*                                 RESULTAT I SAMMA COPYTEXT               
000130*                                 RÄTT = "R" I KDSVAR                     
000140*                                    OCH DATA I IDLTERM, BEPRTLST         
000150*                                 FEL  = "F" I KDSVAR                     
000160*                                    OCH "SAKNAS" I IDLTERM               
000170*                                                                         
000180*                                 2. FINNS DENNA LTERM?                   
000190*                                 MOVE 002 TO PRT-KDCALL                  
000200*                                 MOVE "R22653  " TO PRT-IDLTERM          
000210*                                 CALL W006PRT USING PRT-W006PRT          
000220*                                 RESULTAT I SAMMA COPYTEXT               
000230*                                 RÄTT = "R" I KDSVAR                     
000240*                                    OCH DATA I IDPRTLST,BEPRTLST         
000250*                                 FEL  = "F" I KDSVAR                     
000260*                                    OCH "SAKNAS" I IDPRTLST              
000270*                                                                         
000280*                                 3. VILKEN NODE?                         
000290*                                 MOVE 003 TO PRT-KDCALL                  
000300*                                 MOVE "MRVG    " TO PRT-IDPRTLST         
000310*                                 CALL W006PRT USING PRT-W006PRT          
000320*                                 RESULTAT I SAMMA COPYTEXT               
000330*                                 RÄTT = "R" I KDSVAR                     
000340*                                    OCH DATA I IDNODE , BEPRTLST         
000350*                                 FEL  = "F" I KDSVAR                     
000360*                                    OCH "SAKNAS" I IDNODE                
000370*                                                                         
000380*                                 4. FINNS DENNA NODE?                    
000390*                                 MOVE 004 TO PRT-KDCALL                  
000400*                                 MOVE "R30364  " TO PRT-IDNODE           
000410*                                 CALL W006PRT USING PRT-W006PRT          
000420*                                 RESULTAT I SAMMA COPYTEXT               
000430*                                 RÄTT = "R" I KDSVAR                     
000440*                                    OCH "FINNS" I IDPRTLST               
000450*                                 FEL  = "F" I KDSVAR                     
000460*                                    OCH "SAKNAS" I IDPRTLST              
000470*                                                                         
000480     03 PRT-KDCALL           PIC 9(3).                                    
000490*                                 ANROPSTYP                               
000500*                                 CALL TYPE                               
000510     03 PRT-IDPRTLST         PIC X(8).                                    
000520*                                 LOGISK PRINTER+LISTA IDENTITET          
000530*                                 LOGICAL PRINTER+LIST IDENTITY           
000540     03 PRT-IDLTERM          PIC X(8).                                    
000550*                                 LOGISKT TERMINALNAMN                    
000560*                                 IDENTITY OF LOGICAL TERMINAL            
000570     03 PRT-IDNODE           PIC X(8).                                    
000580*                                 VTAM NODE-NAMN                          
000590*                                 VTAM NODE NAME                          
000600     03 PRT-BEPRTLST         PIC X(25).                                   
000610*                                 LOGISK LISTA+PRINTER BENÄMNING          
000620*                                 LOGICAL PRINTER+LIST NAME               
000630     03 PRT-KDSVAR           PIC X.                                       
000640*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
000650*                                 RETURN CODE FROM PROGRAM                
000660*** END COPY W006PRT   LENGTH=53                                          
