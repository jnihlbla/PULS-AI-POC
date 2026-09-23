//PC35324C JOB (670W4750100W475J100,W100),'CAMELIA',                            
//        CLASS=V,NOTIFY=PC35324,                                               
//        MSGCLASS=H,MSGLEVEL=(1,1)                                             
/*JOBPARM TIME=5,CARDS=0,FORMS=STD                                              
//*+JBS BIND IMG0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W475P112 EXEC W475P112                                                        
//W47512.W47512D1 DD DSN=W475.COT.W47511                                        
//W47512.W47512D2 DD DSN=W475.COT.W47513                                        
//W47512.W47512D3 DD DSN=W475.COT.W47514                                        
//*                                                                             
