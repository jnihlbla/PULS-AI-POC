000010 01  KVAL-W426KSC2.                                                       
000020*                                 LÄNKAREA TILL W426KSC2 -                
000030*                                 GER SVAR PÅ FRÅGA ANTAL TILL KO         
000040*                                 NTROLL                                  
000050     03 KVAL-IDLEVNR         PIC S9(5)           COMP-3.                  
000060*                                 LEVERANTÖRNUMMER                        
000070     03 KVAL-IDARTNR         PIC S9(9)           COMP-3.                  
000080*                                 ARTIKELNUMMER                           
000090     03 KVAL-IDLOPNRM        PIC S9(9)           COMP-3.                  
000100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000110*                                 (0VVDLLLLK)                             
000120     03 KVAL-KDRT            PIC S9(3)           COMP-3.                  
000130*                                 REDOVISNINGSTYP                         
000140     03 KVAL-FLKVAANT        PIC X.                                       
000150*                                 ANTALSKONTROLL UTFÖRD                   
000160     03 KVAL-KDKVAINF        PIC X.                                       
000170*                                 TYP AV KVAL.INFO FÖR ARTIKEL            
000180     03 KVAL-ADKVAULG        PIC X(2).                                    
000190*                                 PLATS UNDERLAG KVAL.KONTROLL            
000200     03 KVAL-KDKVAKTL.                                                    
000210*                                 KVALITETSKONTROLL KOD                   
000220        05 KVAL-KDKVATYP     PIC X.                                       
000230*                                 NORMAL/VERIFIKATIONS KVAL.KONTR         
000240        05 KVAL-IDPROVPL-PRI PIC X.                                       
000250*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
000260        05 KVAL-IDPROVPL-SEK PIC X.                                       
000270*                                 PROVTAGNINGSPLAN SEK.KONTROLL           
000280        05 KVAL-KDKVAULG     PIC X.                                       
000290*                                 UNDERLAG FÖR KVALITETSKONTROLL          
000300     03 KVAL-KVAVIS          PIC S9(7)           COMP-3.                  
000310*                                 AVISERAT ANTAL                          
000320     03 KVAL-KVKVAPRIM       PIC S9(7)           COMP-3.                  
000330*                                 ANTAL TILL PRIMÄRKONTROLL               
000340     03 KVAL-KVKVASEK        PIC S9(7)           COMP-3.                  
000350*                                  ANTAL TILL SEKUNDÄRKONTROLL            
000360     03 KVAL-IDRITN          PIC X(10).                                   
000370*                                 RITNINGSNUMMER                          
000380     03 KVAL-IDAVINR         PIC S9(7)           COMP-3.                  
000390*                                 AVI-NUMMER                              
000400     03 KVAL-TEMRKNTR        PIC X(8).                                    
      *** END COPY W426KSC2    LENGTH=57                                        
