//W371J0RE JOB (670W3710100W371J0RE,W100),'RTN W371V1',                         
//* ÄR UPPDRAGSKODEN OVAN RÄTT?????                                             
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P0RE                                                         
//*                                                                             
