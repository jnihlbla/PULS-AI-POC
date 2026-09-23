000100 01  WF0202I1.                                                            
000200*                                 GENERAL ENTRANCE FOR LINES TO B         
000300*                                 E INVOICED/CREDITED                     
000400*                                 MIDCOPYTEXT FOR LINES TO BE VAL         
000500*                                 IDATE                                   
000600     03 IDLEGSEL             PIC X(4).                                    
000700*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000800*                                 LEGAL SELLER IDENTITY                   
000900     03 IDBUNDLE             PIC X(15).                                   
001000*                                 BUNDLE ID                               
001100*                                 BUNDLE ID                               
001200     03 DAREGDAT             PIC X(8).                                    
001300*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001400*                                 REGISTRATION DATE (YYYYMMDD)            
001500     03 TIREGTID             PIC S9(10)          COMP-3.                  
001600*                                 REGISTRERINGSTID                        
001700*                                 GENERAL REGISTRATION TIME               
001800     03 IDREF                PIC X(15).                                   
001900*                                 REFERENS ID                             
002000*                                 REFERENCE ID                            
002100     03 DAREFDAT             PIC X(8).                                    
002200*                                 REFERENSDATUM (≈≈≈≈MMDD)                
002300*                                 REFERENCE DATE(YYYYMMDD)                
002400     03 IDREFRAD             PIC S9(5)           COMP-3.                  
002500*                                 REFERENSRADSNR                          
002600*                                 REFERENCE LINE NUMBER                   
002700     03 FLFEL                PIC X.                                       
002800*                                 ALLMƒN FELFLAGGA                        
002900*                                 GENERAL ERROR FLAG                      
003000*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  
