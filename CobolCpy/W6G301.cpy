000010 01  FIL-W6G301.                                                          
000020*                                 TRANSAR TILL BATCH FR≈N                 
000030*                                 INLEVERANS ONLINESYSTEM                 
000040*                                 FYSISK NYCKEL: W6G301KY                 
000050*                                 (IDPGM + TIREGDAT + TIKLOCK +           
000060*                                  IDSEKVNR + IDCPYTXT)                   
000070*                                 S÷KBEGREPP: IDPGM, TIREGDAT,            
000080*                                 TIKLOCK, IDSEKVNR, IDCPYTXT             
000090     03 FIL-IDPGM            PIC X(8).                                    
000100*                                 PROGRAM IDENTITET                       
000110*                                 PROGRAM INTENTITY                       
000120     03 FIL-TIREGDAT         PIC S9(7)           COMP-3.                  
000130*                                 REGISTRERINGSDATUM (≈≈MMDD)             
000140*                                 REGISTRATION DATE (YYMMDD)              
000150     03 FIL-TIKLOCK          PIC S9(9)           COMP-3.                  
000160*                                 KLOCKSLAG (TTMMSSTH)                    
000170*                                 TIME OF DAY (HHMMSSTH)                  
000180     03 FIL-IDSEKVNR         PIC S9(3)           COMP-3.                  
000190*                                 GENERELLT SEKVENSNUMMER                 
000200*                                 GENERAL SEQUENCE NUMBER                 
000210     03 FIL-IDCPYTXT.                                                     
000220*                                 COPYTEXT IDENTITET                      
000230*                                 IDENTITY OF A COPYTEXT                  
000240        05 FIL-CT-IDSYSTEM   PIC X(4).                                    
000250*                                 SKAPANDE SYSTEMNUMMER                   
000260*                                 GENERATING SYSTEM NUMBER                
000270        05 FIL-CT-IDPTYP     PIC X(3).                                    
000280*                                 POSTTYP                                 
000290*                                 RECORD TYPE                             
000300        05 FIL-CT-IDVTYP     PIC X.                                       
000310*                                 POSTTYPSVERSION                         
000320*                                 RECORD TYPE VERSION                     
000330     03 FIL-W6G301-DATA      PIC X(200).                                  
      *** END COPY W6G301      LENGTH=227                                       
