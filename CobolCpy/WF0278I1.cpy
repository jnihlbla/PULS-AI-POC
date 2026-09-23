000100 01  REQU-WF0278I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0278             
000300*                                 ERRONEUS BUNDLES DETAIL                 
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-IDBUNDLE-KEY    PIC X(15).                                   
000800*                                 BUNDLE ID                               
000900*                                 BUNDLE ID                               
001000     03 REQU-DAREGDAT-KEY    PIC X(8).                                    
001100*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001200*                                 REGISTRATION DATE (YYYYMMDD)            
001300     03 REQU-TIREGTID-KEY    PIC 9(10).                                   
001400*                                 REGISTRERINGSTID                        
001500*                                 GENERAL REGISTRATION TIME               
001600     03 REQU-IDREF-KEY       PIC X(15).                                   
001700*                                 REFERENS ID                             
001800*                                 REFERENCE ID                            
001900     03 REQU-DAREFDAT-KEY    PIC X(8).                                    
002000*                                 REFERENSDATUM (≈≈≈≈MMDD)                
002100*                                 REFERENCE DATE(YYYYMMDD)                
002200     03 REQU-IDREFRAD-KEY    PIC 9(5).                                    
002300*                                 REFERENSRADSNR                          
002400*                                 REFERENCE LINE NUMBER                   
002500     03 REQU-PAGE-REQUEST    PIC X.                                       
002600*                                 TYP AV PROGRAMBEARBETNING               
002700*                                 TYPE OF PROGRAM ACTION                  
002800     03 REQU-FLRESTBUN       PIC X.                                       
002900*                                 ≈TERSTARTBAR BUNT ?                     
003000*                                 RESTARTABLE BUNDLE ?                    
003100*** END OF VILMAII-COPY LENGTH= 67 BYTES                                  
