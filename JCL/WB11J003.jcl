//WB11J003 JOB (670WB010100WB11J003,W100),'RTN WB11S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTB                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ   NJEVC                                                           
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//* beställt med filnamn    XLSFIL(&XLSFIL)                                     
//*              startvecka TIAOINF-FOM(&TIAOINF-FOM)                           
//*              slutvecka  TIAOINF-TOM(&TIAOINF-TOM)                           
//*              av user    IDUSER(&IDUSER)                                     
//*              S-Uppdrag  SU(&SU)                                             
//*              K-Uppdrag  KU(&KU)                                             
//*              Call-typ   CALL(&CALL)                                         
//*                                                                             
//WB11    EXEC WB11P003                                                         
//WB1103.SYSIN DD *                                                             
&XLSFIL,&TIAOINF-FOM,&TIAOINF-TOM,&IDUSER,&SU,&KU,&CALL,                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WB11J003                                         
