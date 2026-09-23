000010 01  1132-WDGX1132.                                                       
000020*                                 NYA ARTIKLAR FRÅN PV, LV                
000030*                                 PROJEKT-INFORMATION                     
000040*                                 FYSISK NYCKEL: WDGXKEY                  
000050*                                 (IDPROJK + IDPROJOBJ +                  
000060*                                  IDPROJ  + LOWVALUE)                    
000070     03 1132-IDPROJK         PIC X(4).                                    
000080*                                 PROJEKTIDENTITET KONSTRUKTION           
000090*                                 PROJECT IDENTITY KONSTRUCTION           
000100     03 1132-IDPROJOBJ       PIC X(4).                                    
000110*                                 PROJEKTIDENTITET LV OBJEKT              
000120*                                 PROJECT IDENTITY, TRUCK OBJECT          
000130     03 1132-IDPROJ          PIC X(4).                                    
000140*                                 PARTS PROJEKTIDENTITET                  
000150*                                 PARTS PROJECT IDENTITY                  
000160     03 1132-LOWVALUE        PIC X(3).                                    
000170     03 1132-IDLKTO          PIC S9(7)           COMP-3.                  
000180*                                 LAGERKONTO (FFHHHUU)                    
000190*                                 STOCK ACCOUNT (CCMMMSS)                 
000200     03 1132-KVNYART         PIC S9(5)           COMP-3.                  
000210*                                 PROGNOS NYA ARTIKLAR                    
000220*                                 PROGNOS NEW PARTS                       
000230     03 1132-KVNYRES         PIC S9(5)           COMP-3.                  
000240*                                 PROGNOS NYA RESERVDELAR                 
000250*                                 PROGNOS NEW SPARE PARTS                 
000260     03 1132-RESLJUST-C1     PIC S9(2)V9(1)      COMP-3.                  
000270*                                 SÄKERHETSLAGER JUST C1                  
000280*                                 SAFETY STOCK ADJUST C1                  
000290     03 1132-RESLJUST-C2     PIC S9(2)V9(1)      COMP-3.                  
000300*                                 SÄKERHETSLAGER JUST C2                  
000310*                                 SAFETY STOCK ADJUST C2                  
000320     03 1132-TIFINLEV        PIC S9(7)           COMP-3.                  
000330*                                 PUBLICERINGSDATUM  (AAMMDD)             
000340*                                 DATE 1:ST GOODS REC (YYMMDD)            
000350     03 1132-TIGENORD        PIC S9(7)           COMP-3.                  
000360*                                 GENERELL TID FÖR ORDERSLÄPP             
000370*                                 GENERAL TIME FOR ORDER RELEASE          
000380     03 1132-TIPRODSTA       PIC S9(7)           COMP-3.                  
000390*                                 PRODUKTIONS START AV VAGN               
000400*                                 PRODUCTION START                        
000410     03 1132-TIPROJSTO       PIC S9(7)           COMP-3.                  
000420*                                 PROJEKTSTOPP BASLAGER                   
000430*                                 PROJECT STOP BASIC STOCK                
000440     03 1132-TISTAMREG       PIC S9(7)           COMP-3.                  
000450*                                 STARTTID 4 VECKORS REGELN               
000460*                                 START TIME 4 WEEKS RULES                
000470     03 1132-TIUPPDAT        PIC S9(7)           COMP-3.                  
000480*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
000490*                                 UPDATING DATE     (YYMMDD)              
000500     03 1132-TIPROINF-FOM    PIC S9(7)           COMP-3.                  
000510*                                 DATUM PRODUKTIONS INFÖRANDE             
000520*                                 INOM PROJEKT                            
000530*                                 ENG***                                  
000540     03 1132-TIPROINF-TOM    PIC S9(7)           COMP-3.                  
000550*                                 DATUM PRODUKTIONS INFÖRANDE             
000560*                                 INOM PROJEKT                            
000570*                                 ENG***                                  
000580     03 1132-KDAGE           PIC X.                                       
000590*                                 AGE-CODE                                
000600*                                 AGE-CODE                                
000610     03 FILLER               PIC X(3).                                    
000620*** END COPY WDGX1132  LENGTH=65                                          
