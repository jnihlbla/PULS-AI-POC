000010 01  XBMS-KAM.                                                            
000020*                                 PARTS INFORMATION TRANS FROM            
000030*                                 MARKET COMPANY                          
000040     03 FILLER               PIC X.                                       
000050     03 IDPROMR.                                                          
000060*                                 PRICE AREA                              
000070        05 IDMARKBO          PIC X.                                       
000080*                                 MARKET COMPANY CODE                     
000090*                                 A = VCS                                 
000100*                                 B = VCEM                                
000110*                                 C = NORDIC WITHOUT SWEDEN               
000120*                                 D = VCUK                                
000130*                                 E = VCNA                                
000140*                                 F = VCI                                 
000150*                                 G = VCAS                                
000160        05 IDPROMRN          PIC X(2).                                    
000170*                                 PRICE AREA SERIALNUMBER                 
000180     03 TISTADAT-XBMS        PIC X(6).                                    
000190     03 TISTODAT-XBMS        PIC X(6).                                    
000200     03 KDARTKAM-XBMS        PIC X(4).                                    
000210*                                 TRANSFER CONDITION CODE                 
000220     03 FILLER               PIC X.                                       
000230     03 REARTRAB-DO-XBMS     PIC S9(3)V9(2)      COMP-3.                  
000240*                                 STOCK BALANCE/PERIOD REQ F KIT          
000250     03 REARTRAB-MO-XBMS     PIC S9(3)V9(2)      COMP-3.                  
000260*                                 STOCK BALANCE/PERIOD REQ F KIT          
000270     03 FILLER               PIC X(5).                                    
      *** END COPY W33520X     LENGTH=32                                        
