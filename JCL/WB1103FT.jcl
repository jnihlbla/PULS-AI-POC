//WB1103FT JOB (640WB010100WB1103FT,W100),'RTN WB11S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ  NJEVC                                                            
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* beställt med filnamn    XLSFIL(&XLSFIL)                                     
//*              startvecka TIAOINF-FOM(&TIAOINF-FOM)                           
//*              slutvecka  TIAOINF-TOM(&TIAOINF-TOM)                           
//*              av user    IDUSER(&IDUSER)                                     
//*              S-Uppdrag  SU(&SU)                                             
//*              K-Uppdrag  KU(&KU)                                             
//*              Call-typ   CALL(&CALL)                                         
//*                                                                             
//* Nedan, gamla PATH:en                                                        
//*PATHOUT='/volvo/vccsroot/wb01/qase/data/&XLSFIL.'                            
//*                                                                             
//COPY EXEC W001HFSC,CONV='(BPXFX311)',                                         
//            DSIN='WB11.WB11S1.WB1103(+0)',                                    
// PATHOUT='/app/vccs/qase/wb01/data/&XLSFIL.'                                  
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WB1103FT                                         
